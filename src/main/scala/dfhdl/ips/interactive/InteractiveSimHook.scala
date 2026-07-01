package dfhdl.ips.interactive

import dfhdl.tools.{ForeignSimHook, ForeignSimContext}
import dfhdl.options.SimulatorOptions
import dfhdl.tools.toolsCore.DFToolsImage
import java.net.{InetAddress, ServerSocket}

/** The `interactive` IP-specific simulation context. Shared by [[interactive_ctrl]] and
  * [[interactive_flag]] — one process-global backend, one socket, one viewer — so a single context
  * configures the whole run regardless of which IP it came from.
  */
final class InteractiveSimContext(
    ipName: String,
    ipDir: os.Path,
    topName: String,
    platformID: Option[String],
    // test override: point the backend at an already-listening server (the test plays the viewer
    // over a raw socket); when set, launch NO GUI viewer.
    val streamOverride: Option[String]
) extends ForeignSimContext(ipName, ipDir, topName, platformID):
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
  * config-driven graphical panel). It is launched **only when the top design carries a
  * `@platformID(...)` annotation**, as `fpga-isv --example <platformID>` (the platform name — e.g.
  * `ulx3s` — is exactly a bundled fpga-isv board). It runs on the host PATH locally, or inside the
  * DFTools `hmi` image (with X11) under dftools — matching wherever the simulator runs, so both
  * share the same loopback (the sim and hmi containers share the WSL network namespace). With no
  * `@platformID` the sim still runs (the backend is a no-op while no viewer is attached).
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
    val streamOverride =
      Option(System.getProperty(s"$sysPropPrefix.stream")).map(_.trim).filter(_.nonEmpty)
    new InteractiveSimContext(
      base.ipName,
      base.ipDir,
      base.topName,
      base.platformID,
      streamOverride
    )
  end context

  override def onSimStart(ctx: InteractiveSimContext)(using SimulatorOptions): Unit =
    // test mode (streamOverride): the test already listens and plays the viewer itself — nothing to do.
    // Otherwise launch fpga-isv only when the top is `@platformID`-annotated (its board).
    if (ctx.streamOverride.isEmpty && ctx.platformID.isDefined)
      // pick a free loopback port for the viewer (fpga-isv) to bind; the backend reconnects to it.
      ctx.port =
        val s = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
        try s.getLocalPort
        finally s.close()
      // launch off-thread: an image pull / GUI startup must not block the sim launch, and the
      // backend keeps (re)connecting so the viewer can come up at its own pace.
      val t = new Thread(() => launchViewer(ctx), "interactive-viewer")
      t.setDaemon(true)
      t.start()
      ctx.worker = t
  end onSimStart

  override def simEnv(ctx: InteractiveSimContext): Map[String, String] =
    // point the backend at the test server, the launched viewer's port, or nowhere (no viewer).
    ctx.streamOverride.orElse(Option.when(ctx.port >= 0)(s"127.0.0.1:${ctx.port}")) match
      case Some(stream) => Map("INTERACTIVE_STREAM" -> stream)
      case None         => Map.empty

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

  // Launch `fpga-isv --example <platformID>` as the viewer/server, on the host PATH locally or inside
  // the DFTools `hmi` image (with X11) under dftools — matching where the simulator runs. fpga-isv
  // binds 127.0.0.1:port and the backend connects to it. Failure (e.g. fpga-isv not installed) is
  // non-fatal: the sim runs without a viewer.
  private def launchViewer(ctx: InteractiveSimContext)(using to: SimulatorOptions): Unit =
    try
      val dftools = to.runLocation == dfhdl.options.ToolOptions.Location.dftools
      // the simulator's mounted cwd ($PWD in the image): strip the IP's `dfhdl-ips/<ip>` resource path.
      val execDir = ctx.ipDir / os.up / os.up
      val cmd =
        Seq("fpga-isv", "--host", "127.0.0.1", "--port", ctx.port.toString) ++
          ctx.platformID.toSeq.flatMap(id => Seq("--example", id))
      val argv = if (dftools) DFToolsImage.execArgv("hmi", cmd, withX11 = true) else cmd
      val p = os.proc(argv).spawn(
        cwd = execDir,
        stdin = os.Inherit,
        stdout = os.ProcessOutput.Readlines(_ => ()), // drain so the pipe never back-pressures
        mergeErrIntoOut = true
      )
      ctx.viewerProc = p
    catch case _: Throwable => () // no viewer available; the sim still runs
  end launchViewer
end InteractiveSimHook
