package dfhdl.ips.video.vga
import dfhdl.*

/** A software VGA monitor foreign IP wrapping
  * [[https://github.com/DFiantWorks/vga-monitor-sim vga-monitor-sim]]: wire your design's VGA
  * signaling into it and, during simulation, its bundled C++ backend (loaded into the simulator via
  * a DPI/VPI/VHPI shim) reconstructs frames and serves them over TCP to a standard viewer launched
  * by [[VgaMonitorSimHook]] (`ffplay` live / `ffmpeg` to capture a frame).
  *
  * The monitor is clockless - it auto-locks sync polarity, pixel clock, resolution and offset
  * purely from the signal. [[COLOR_BITS]] sizes the `r`/`g`/`b` channels (default 8, passed through
  * to the wrapper's `COLOR_BITS` parameter/generic); `hsync`/`vsync` are single bits. The bundled
  * binaries and HDL wrappers are unversioned - the wrapped release version lives only in
  * [[vga_monitor.version]] (and the download archive name).
  *
  * Since vga-monitor-sim v1.0.0 the backend is the TCP server: it streams the self-describing `ppm`
  * (P6) format, so the viewer recovers each frame's resolution from the stream itself (no fixed
  * default) and connects/reconnects at any time while the simulation runs unaffected.
  */
class vga_monitor(
    val COLOR_BITS: Int <> CONST = 8
) extends EDBlackBox.ForeignIP:
  val r = Bits(COLOR_BITS) <> IN
  val g = Bits(COLOR_BITS) <> IN
  val b = Bits(COLOR_BITS) <> IN
  val hsync = Bit <> IN
  val vsync = Bit <> IN

  override protected def dpiLib = "vga_monitor_dpi"
  override protected def vpiModule = "vga_monitor"
  override protected def vhpiLib = "vga_monitor_vhpi"
  override protected def simHookClass = "dfhdl.ips.video.vga.VgaMonitorSimHook"
end vga_monitor

object vga_monitor:
  /** The wrapped vga-monitor-sim release, set from `build.sbt` (`vgaMonitorVersion`) via the
    * generated `vga-monitor.properties` resource (mirrors core's `version.properties` and lib's
    * `dftools.properties`). Bump it in `build.sbt`.
    */
  val version: String =
    val props = new java.util.Properties()
    // close the stream: a leaked handle blocks the build from re-copying the resource on Windows
    // (AccessDenied during copyResources).
    val inputStream = getClass.getClassLoader.getResourceAsStream("vga-monitor.properties")
    try props.load(inputStream)
    finally inputStream.close()
    props.getProperty("vga-monitor.version")
end vga_monitor
