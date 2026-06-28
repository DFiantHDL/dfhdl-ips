package dfhdl.ips.interactive
import dfhdl.*

/** A design-driven OUTPUT foreign IP wrapping
  * [[https://github.com/DFiantWorks/interactive-sim interactive-sim]]: drop it anywhere in a
  * design, unconnected to anything else, and whenever [[value]] changes it is pushed to the viewer
  * under [[NAME]] (LED / 7-segment / status word). Pair it with [[interactive_ctrl]] for
  * viewer-driven inputs; both share one backend singleton and one viewer over a single TCP socket
  * ([[InteractiveSimHook]]).
  *
  * [[NAME]] is the channel id on the wire and the viewer label; it must be unique across the whole
  * simulation. [[WIDTH]] sizes [[value]]. Unlike [[interactive_ctrl]] the wrapper is event-driven
  * (it emits on every [[value]] change), so it needs no simulator timing support of its own.
  */
class interactive_flag(
    val NAME: String <> CONST,
    val WIDTH: Int <> CONST = 1
) extends EDBlackBox.ForeignIP:
  val value = Bits(WIDTH) <> IN

  // shares the one `interactive` bundle with interactive_ctrl (same backend + VHDL package)
  override protected def resourcePath = "dfhdl-ips/interactive"
  override protected def dpiLib = "interactive_dpi"
  override protected def vpiModule = "interactive"
  override protected def vhpiLib = "interactive_vhpi"
  override protected def simHookClass = "dfhdl.ips.interactive.InteractiveSimHook"
end interactive_flag
