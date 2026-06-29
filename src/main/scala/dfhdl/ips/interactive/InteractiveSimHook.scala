package dfhdl.ips.interactive

import dfhdl.tools.{ForeignSimHook, ForeignSimContext}
import dfhdl.options.SimulatorOptions
import dfhdl.tools.toolsCore.DFToolsImage
import java.net.{InetAddress, ServerSocket}
import java.nio.charset.StandardCharsets.UTF_8

/** The `interactive` IP-specific simulation context. Shared by [[interactive_ctrl]] and
  * [[interactive_flag]] — one process-global backend, one socket, one viewer — so a single context
  * configures the whole run regardless of which IP it came from.
  */
final class InteractiveSimContext(
    ipName: String,
    ipDir: os.Path,
    topName: String,
    // test override: point the backend at an already-listening server (the test plays the viewer
    // over a raw socket); when set, launch NO GUI viewer.
    val streamOverride: Option[String],
    // fpga-isv board/panel config JSON (`--config`)
    val viewerConfig: Option[String],
    // fpga-isv bundled example board (`--example`, e.g. "ulx3s")
    val viewerExample: Option[String]
) extends ForeignSimContext(ipName, ipDir, topName):
  // per-run viewer state, populated during onSimStart / the worker thread
  var port: Int = -1
  var worker: Thread = compiletime.uninitialized
  @volatile var viewerProc: os.SubProcess = compiletime.uninitialized
  @volatile var stopViewer: Boolean = false
end InteractiveSimContext

/** Simulation hook for the `interactive` foreign-IP bundle ([[interactive_ctrl]] +
  * [[interactive_flag]]).
  *
  * The interactive-sim backend loaded into the simulator is the TCP client: it keeps (re)connecting
  * to `INTERACTIVE_STREAM=host:port` — the viewer's address — and exchanges newline-delimited JSON
  * (sim->viewer `reg`/`flag`/`close`/`time`, viewer->sim `{"name","val"}`). The viewer is the
  * server.
  *
  * The viewer is [[https://github.com/DFiantWorks/interactive-sim-viewer fpga-isv]] (a
  * config-driven graphical panel). This hook just launches it as a client of the active
  * tools-location: on the host PATH locally, or inside the DFTools `hmi` image (with X11) under
  * dftools — matching wherever the simulator runs, so both share the same loopback (the sim and hmi
  * containers share the WSL network namespace).
  *
  * fpga-isv needs a board config. By default the hook auto-loads `<topName>.isv.json`, searched in
  * the project resources (on the classpath, alongside the top design) then the current working
  * directory, and COMMITS it to the sim folder (`sandbox/<topName>/<topName>.isv.json` —
  * host-readable and mounted in dftools). An explicit
  * `-Ddfhdl.ips.interactive.viewer=<config.json>` overrides the search;
  * `-Ddfhdl.ips.interactive.example=<name>` (e.g. `ulx3s`) selects a bundled board instead. With no
  * board configured the sim still runs (the backend is a no-op while no viewer is attached).
  *
  * For automated testing the viewer is replaced by a raw socket exchange in the test itself:
  * setting `-Ddfhdl.ips.interactive.stream=host:port` makes this hook point the backend at that
  * already-listening server and launch no GUI.
  *
  * One hook instance serves the whole simulation (the tools layer dedups it per hook class), so it
  * covers every `interactive_ctrl`/`interactive_flag` channel over the one socket.
  */
object InteractiveSimHook extends ForeignSimHook[InteractiveSimContext]:
  private val sysPropPrefix = "dfhdl.ips.interactive"
  // at sim end, give the launcher thread a moment to settle before we close the viewer.
  private val viewerStopJoinMs = 2000L

  def context(base: ForeignSimContext): InteractiveSimContext =
    def prop(k: String): Option[String] =
      Option(System.getProperty(s"$sysPropPrefix.$k")).map(_.trim).filter(_.nonEmpty)
    new InteractiveSimContext(
      base.ipName,
      base.ipDir,
      base.topName,
      prop("stream"),
      prop("viewer"),
      prop("example")
    )
  end context

  override def onSimStart(ctx: InteractiveSimContext)(using SimulatorOptions): Unit =
    // test mode: the test already listens on streamOverride and plays the viewer itself — nothing to do.
    if (ctx.streamOverride.isEmpty)
      // pick a free loopback port for the viewer (fpga-isv) to bind; the backend reconnects to it.
      ctx.port =
        val s = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
        try s.getLocalPort
        finally s.close()
      // launch off-thread: an image pull / GUI startup must not block the sim launch, and the
      // backend keeps (re)connecting so the viewer can come up at its own pace. The board is
      // resolved (and committed) inside the worker, which logs a hint if none is found.
      val t = new Thread(() => launchViewer(ctx), "interactive-viewer")
      t.setDaemon(true)
      t.start()
      ctx.worker = t
  end onSimStart

  override def simEnv(ctx: InteractiveSimContext): Map[String, String] =
    Map("INTERACTIVE_STREAM" -> ctx.streamOverride.getOrElse(s"127.0.0.1:${ctx.port}"))

  override def onSimEnd(ctx: InteractiveSimContext)(using SimulatorOptions): Unit =
    // close the viewer when the sim ends (finished or Ctrl+C'd — onSimEnd runs in
    // withForeignSimHooks' finally). Best-effort: kills the local process; in dftools it kills the
    // host launcher (the in-container fpga-isv may linger until the user closes it).
    ctx.stopViewer = true
    Option(
      ctx.worker
    ).foreach(_.join(viewerStopJoinMs)) // let the launcher publish viewerProc first
    Option(ctx.viewerProc).foreach(p =>
      try if (p.wrapped.isAlive) p.wrapped.destroyForcibly()
      catch case _: Throwable => ()
    )
  end onSimEnd

  // Launch fpga-isv as the viewer/server, on the host PATH locally or inside the DFTools `hmi` image
  // (with X11) under dftools — matching where the simulator runs. fpga-isv binds 127.0.0.1:port and
  // the backend connects to it. Failure (e.g. fpga-isv not installed) is non-fatal: the sim runs
  // without a viewer.
  private def launchViewer(ctx: InteractiveSimContext)(using to: SimulatorOptions): Unit =
    try
      val dftools = to.runLocation == dfhdl.options.ToolOptions.Location.dftools
      // the simulator's mounted cwd ($PWD in the image): strip the IP's `dfhdl-ips/<ip>` resource path.
      val execDir = ctx.ipDir / os.up / os.up
      resolveBoardArgs(ctx, execDir, dftools) match
        case None =>
          // nothing to show; the sim still runs (the backend no-ops while no viewer is attached).
          System.err.println(
            s"[interactive] no viewer board found; add a ${ctx.topName}.isv.json to the project " +
              s"resources or run dir (or set -D$sysPropPrefix.example=<name>, e.g. ulx3s) to launch fpga-isv"
          )
        case Some(board) =>
          val cmd =
            Seq("fpga-isv", "--host", "127.0.0.1", "--port", ctx.port.toString) ++ board
          val argv = if (dftools) DFToolsImage.execArgv("hmi", cmd, withX11 = true) else cmd
          val p = os.proc(argv).spawn(
            cwd = execDir,
            stdin = os.Inherit,
            stdout = os.ProcessOutput.Readlines(_ => ()), // drain so the pipe never back-pressures
            mergeErrIntoOut = true
          )
          ctx.viewerProc = p
      end match
    catch case _: Throwable => () // no viewer available; the sim still runs
  end launchViewer

  // Resolve fpga-isv's board arguments. A `<topName>.isv.json` config is sought, in order, from:
  //   1. an explicit `-Ddfhdl.ips.interactive.viewer=<path>` override,
  //   2. the project resources (classpath, alongside the top design),
  //   3. the current working directory,
  // and, when found, is COMMITTED to `execDir/<topName>.isv.json` (the `sandbox/<topName>` folder —
  // host-readable and mounted as the cwd in dftools) and passed as `--config` (relative to that cwd
  // in dftools so the in-container fpga-isv can read it). Failing all of those, fall back to a bundled
  // `-Ddfhdl.ips.interactive.example=<name>`. Returns None when no board is configured at all.
  private[interactive] def resolveBoardArgs(
      ctx: InteractiveSimContext,
      execDir: os.Path,
      dftools: Boolean
  ): Option[Seq[String]] =
    val name = s"${ctx.topName}.isv.json"
    def tryReadFile(p: os.Path): Option[String] =
      try if (os.exists(p)) Some(os.read(p)) else None
      catch case _: Throwable => None
    def resourceJson: Option[String] =
      Option(getClass.getClassLoader.getResourceAsStream(name)).map { is =>
        try new String(is.readAllBytes(), UTF_8)
        finally is.close()
      }
    val json: Option[String] =
      ctx.viewerConfig.flatMap(c => tryReadFile(os.Path(c, os.pwd))) // explicit override path
        .orElse(resourceJson) // project resources (classpath)
        .orElse(tryReadFile(os.pwd / name)) // current working directory
    json match
      case Some(content) =>
        val committed = execDir / name
        os.write.over(committed, content)
        val arg = if (dftools) committed.relativeTo(execDir).toString else committed.toString
        Some(Seq("--config", arg))
      case None =>
        ctx.viewerExample.map(ex => Seq("--example", ex))
  end resolveBoardArgs
end InteractiveSimHook
