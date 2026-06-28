package dfhdl.ips.interactive
import dfhdl.*

/** A viewer-driven INPUT foreign IP wrapping
  * [[https://github.com/DFiantWorks/interactive-sim interactive-sim]]: drop it anywhere in a
  * design, unconnected to anything else, and during simulation its bundled C++ backend (loaded into
  * the simulator via a DPI/VPI/VHPI shim) streams the latest viewer-set value for [[NAME]] onto
  * [[value]]. Pair it with [[interactive_flag]] for design-driven outputs; both share one backend
  * singleton and one viewer over a single TCP socket ([[InteractiveSimHook]]).
  *
  * The component is clockless: it self-paces on its own [[POLL_US]] timer (microseconds, default 1
  * ms), so it is asynchronous to the rest of the design. [[NAME]] is the channel id on the wire and
  * the viewer label; it must be unique across the whole simulation. [[WIDTH]] sizes [[value]].
  *
  * Because the wrapper uses delay (`#`/`wait for`) controls, [[needsTiming]] is set so Verilator
  * builds with `--timing` (the other simulators handle delays natively).
  */
class interactive_ctrl(
    val NAME: String <> CONST,
    val WIDTH: Int <> CONST = 1,
    val POLL_US: Int <> CONST = 1000
) extends EDBlackBox.ForeignIP:
  val value = Bits(WIDTH) <> OUT

  // both interactive IPs resolve to ONE shared bundle (one C++ singleton backend + one VHDL package),
  // so they relay the same resource path and FFI lib base names (see InstMode.BlackBox.Source.ForeignIP)
  override protected def resourcePath = "dfhdl-ips/interactive"
  override protected def dpiLib = "interactive_dpi"
  override protected def vpiModule = "interactive"
  override protected def vhpiLib = "interactive_vhpi"
  // the poll loop uses `#`/`wait for` delays -> Verilator must build with `--timing`
  override protected def needsTiming = true
  override protected def simHookClass = "dfhdl.ips.interactive.InteractiveSimHook"
end interactive_ctrl
