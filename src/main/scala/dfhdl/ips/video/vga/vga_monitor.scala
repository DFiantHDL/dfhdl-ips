package dfhdl.ips.video.vga
import dfhdl.*

/** A software VGA monitor foreign IP wrapping
  * [[https://github.com/DFiantWorks/vga-monitor-sim vga-monitor-sim]]: wire your design's VGA
  * signaling into it and, during simulation, its bundled C++ backend (loaded into the simulator via
  * a DPI/VPI/VHPI shim) reconstructs frames and streams them to a viewer ([[VgaMonitorSimHook]]).
  *
  * The monitor is clockless and parameterless - it auto-locks sync polarity, pixel clock,
  * resolution and offset purely from the signal. The `r`/`g`/`b` ports are 8-bit; `hsync`/`vsync`
  * are single bits. The per-FFI library base names embed the wrapped release version (see
  * [[vga_monitor.version]]).
  */
class vga_monitor extends EDBlackBox.ForeignIP:
  val r = Bits(8) <> IN
  val g = Bits(8) <> IN
  val b = Bits(8) <> IN
  val hsync = Bit <> IN
  val vsync = Bit <> IN

  override protected def dpiLib = s"vga_monitor_dpi_${vga_monitor.verSuffix}"
  override protected def vpiModule = s"vga_monitor_${vga_monitor.verSuffix}"
  override protected def vhpiLib = s"vga_monitor_vhpi_${vga_monitor.verSuffix}"
  override protected def simHookClass = "dfhdl.ips.video.vga.VgaMonitorSimHook"
end vga_monitor

object vga_monitor:
  /** The wrapped vga-monitor-sim release. Keep in sync with `vgaMonitorVersion` in `build.sbt`. */
  final val version = "0.2.0"
  // version token embedded in the bundled binary file names, e.g. `v0_2_0`
  final val verSuffix = "v" + version.replace('.', '_')
