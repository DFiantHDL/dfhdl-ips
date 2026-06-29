package dfhdl.ips.video.vga

import dfhdl.tools.{ForeignSimHook, ForeignSimContext}
import dfhdl.options.SimulatorOptions
import dfhdl.tools.toolsCore.DFToolsImage
import java.net.{InetAddress, ServerSocket}
import java.util.concurrent.TimeUnit

/** The `vga_monitor` IP-specific simulation context: capture config (read from system properties)
  * plus the per-run viewer state. Carried through the whole hook lifecycle.
  */
final class VgaMonitorSimContext(
    ipName: String,
    ipDir: os.Path,
    topName: String,
    val captureToOpt: Option[os.Path],
    val captureFrame: Int
) extends ForeignSimContext(ipName, ipDir, topName):
  // per-run viewer state, populated during onSimStart / the worker thread
  var port: Int = -1
  var worker: Thread = compiletime.uninitialized
  @volatile var viewerProc: os.SubProcess = compiletime.uninitialized
  // set on sim end so a still-retrying worker stops trying to (re)connect
  @volatile var stopViewer: Boolean = false
end VgaMonitorSimContext

/** Simulation hook for the `vga_monitor` foreign IP.
  *
  * Since vga-monitor-sim v1.0.0 the backend loaded into the simulator is the TCP **server**: it
  * binds+listens on `VGA_MONITOR_STREAM=host:port` and serves each finished frame as
  * self-describing binary PPM (`VGA_MONITOR_FORMAT=ppm`, a `P6\n<W> <H>\n255\n` header + raw RGB),
  * to whichever viewer connects — at any time, reconnecting freely while the simulation runs
  * unaffected.
  *
  * So this hook no longer decodes frames or runs a windowing toolkit; it just launches a standard
  * viewer as a **client** of that stream and points it at `127.0.0.1:port`:
  *   - interactive (default): `ffplay` shows the live frames in a window;
  *   - capture (test): `ffmpeg` grabs the chosen frame off the socket to a PNG.
  *
  * The viewer runs locally (host PATH) or, when `tools-location` is `dftools`, inside the DFTools
  * `hmi` image (with X11 forwarding for `ffplay`) — matching wherever the simulator itself runs, so
  * both share the same loopback (the sim container and the hmi container share the WSL network
  * namespace). A single monitor instance is supported.
  */
object VgaMonitorSimHook extends ForeignSimHook[VgaMonitorSimContext]:
  // VGA_MONITOR_FRAMES is set this many frames ABOVE the captured frame in capture mode: the backend
  // exit(0)s (ending the sim) the instant it hits the limit, which would race/reset the socket while
  // the grabber is still reading if the limit were the captured frame itself. With margin the grabber
  // reads its frame from a still-live stream and finishes first.
  private val captureFrameMargin = 8
  // give the viewer this long to connect: the backend only starts listening after the sim process
  // spawns (post onSimStart), and in dftools the hmi image may need a cold pull on first use.
  private val viewerConnectDeadlineNs = 120L * 1000 * 1000 * 1000
  private val retryBackoffMs = 250L
  // how long to treat a surviving ffplay as "connected" (a refused connection exits fast instead).
  private val interactiveGraceMs = 800L
  // capture: the grabber has normally already written the PNG by sim end; this only bounds a stuck one.
  private val captureFinishJoinMs = 15000L

  // build our context, reading the IP-specific capture config from system properties
  def context(base: ForeignSimContext): VgaMonitorSimContext =
    val captureToOpt =
      Option(System.getProperty(s"dfhdl.ips.${base.ipName}.capture"))
        .filter(_.nonEmpty)
        .map(os.Path(_, os.pwd))
    val captureFrame =
      Option(System.getProperty(s"dfhdl.ips.${base.ipName}.captureFrame"))
        .flatMap(_.toIntOption)
        .getOrElse(0)
    new VgaMonitorSimContext(base.ipName, base.ipDir, base.topName, captureToOpt, captureFrame)
  end context

  override def onSimStart(ctx: VgaMonitorSimContext)(using SimulatorOptions): Unit =
    // pick a free loopback port for the backend to bind; reused as-is in the WSL stack under dftools.
    ctx.port =
      val s = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
      try s.getLocalPort
      finally s.close()
    // launch the viewer from a background thread: the backend is not listening yet (the sim process
    // starts only after this returns), so the viewer is (re)spawned until it connects.
    val t = new Thread(() => runViewer(ctx), "vga-monitor-viewer")
    t.setDaemon(true)
    t.start()
    ctx.worker = t
  end onSimStart

  override def simEnv(ctx: VgaMonitorSimContext): Map[String, String] =
    // request the self-describing PPM stream so the viewer recovers each frame's geometry from the
    // per-frame P6 header (ffmpeg/ffplay `image2pipe`/`ppm` consume it directly, resyncing on P6).
    val base = Map(
      "VGA_MONITOR_STREAM" -> s"127.0.0.1:${ctx.port}",
      "VGA_MONITOR_FORMAT" -> "ppm"
    )
    // capture mode: bound the run a few frames above the captured one (see captureFrameMargin).
    ctx.captureToOpt match
      case Some(_) => base +
          ("VGA_MONITOR_FRAMES" -> (ctx.captureFrame + captureFrameMargin).toString)
      case None => base
  end simEnv

  override def onSimEnd(ctx: VgaMonitorSimContext)(using SimulatorOptions): Unit =
    ctx.stopViewer = true
    ctx.captureToOpt match
      case Some(_) =>
        // wait for the grabber to flush its PNG, then make sure nothing is left running
        Option(ctx.worker).foreach(_.join(captureFinishJoinMs))
        Option(ctx.viewerProc).foreach(p =>
          try if (p.wrapped.isAlive) p.wrapped.destroyForcibly()
          catch case _: Throwable => ()
        )
      case None => () // leave the ffplay window open so the user can inspect the final frame
  end onSimEnd

  // (Re)spawn the viewer until it connects, the deadline passes, or the run ends. In capture mode the
  // grabber connects, writes the frame, and exits 0; an interactive viewer stays alive once connected.
  // A refused connection (backend not listening yet) exits fast in both, which drives the retry.
  private def runViewer(ctx: VgaMonitorSimContext)(using SimulatorOptions): Unit =
    val deadlineNs = System.nanoTime + viewerConnectDeadlineNs
    var settled = false
    while (!settled && !ctx.stopViewer && System.nanoTime < deadlineNs)
      val proc =
        try Some(spawnViewer(ctx))
        catch case _: Throwable => None
      proc match
        case None =>
          sleep(retryBackoffMs) // viewer/image not launchable right now; back off and retry
        case Some(p) =>
          ctx.viewerProc = p
          ctx.captureToOpt match
            case Some(path) =>
              val code = p.wrapped.waitFor()
              if (code == 0 && os.exists(path) && os.size(path) > 0) settled = true
              else sleep(retryBackoffMs)
            case None =>
              // still running after the grace window => connected; leave it up.
              val exited = p.wrapped.waitFor(interactiveGraceMs, TimeUnit.MILLISECONDS)
              if (!exited) settled = true
              else sleep(retryBackoffMs)
      end match
    end while
  end runViewer

  // Spawn one viewer attempt: ffplay (interactive) or ffmpeg (capture), connecting to the backend's
  // loopback stream. Runs on the host PATH locally, or inside the DFTools `hmi` image (with X11 for
  // ffplay) when tools-location is dftools — matching where the simulator runs.
  private def spawnViewer(ctx: VgaMonitorSimContext)(using to: SimulatorOptions): os.SubProcess =
    val dftools = to.runLocation == dfhdl.options.ToolOptions.Location.dftools
    // the simulator's mounted cwd ($PWD in the image): strip the IP's `dfhdl-ips/<ip>` resource path.
    val execDir = ctx.ipDir / os.up / os.up
    val stream = s"tcp://127.0.0.1:${ctx.port}"
    val (cmd, withX11) = ctx.captureToOpt match
      case Some(path) =>
        // write relative to the mounted cwd in dftools so the PNG is host-readable; absolute locally.
        val out = if (dftools) path.relativeTo(execDir).toString else path.toString
        val select =
          if (ctx.captureFrame > 0) Seq("-vf", s"select=eq(n\\,${ctx.captureFrame})")
          else Seq.empty
        val c = Seq(
          "ffmpeg", "-hide_banner", "-loglevel", "error",
          "-f", "image2pipe", "-vcodec", "ppm", "-i", stream
        ) ++ select ++ Seq("-frames:v", "1", "-y", out)
        (c, false)
      case None =>
        val c = Seq(
          "ffplay",
          "-hide_banner",
          "-loglevel",
          "error",
          "-f",
          "image2pipe",
          "-vcodec",
          "ppm",
          "-window_title",
          s"VGA Monitor - ${ctx.topName}",
          "-i",
          stream
        )
        (c, true)
    val argv = if (dftools) DFToolsImage.execArgv("hmi", cmd, withX11) else cmd
    os.proc(argv).spawn(
      cwd = execDir,
      stdin = os.Inherit,
      // drain output (ffplay/ffmpeg are quiet at -loglevel error) so the pipe never back-pressures.
      stdout = os.ProcessOutput.Readlines(_ => ()),
      mergeErrIntoOut = true
    )
  end spawnViewer

  private def sleep(ms: Long): Unit =
    try Thread.sleep(ms)
    catch case _: InterruptedException => Thread.currentThread().interrupt()
end VgaMonitorSimHook
