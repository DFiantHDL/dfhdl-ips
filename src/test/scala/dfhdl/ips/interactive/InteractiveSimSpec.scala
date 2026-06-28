package dfhdl.ips.interactive

import dfhdl.*
import dfhdl.options.{CompilerOptions, SimulatorOptions}
import dfhdl.tools.simulators
import dfhdl.tools.toolsCore.{VerilogSimulator, VHDLSimulator}
import java.io.File.separatorChar as S

/** A self-checking interactive round-trip: a viewer-driven [[interactive_ctrl]] ("sw") feeds the
  * design, which mirrors it (one registered cycle) onto a design-driven [[interactive_flag]]
  * ("led"). The only path from `sw` to `led` is through this DFHDL design, so the viewer reading
  * back the value it pushed proves the full bidirectional viewer<->backend<->design loop.
  *
  * The run length is data-driven, not fixed: it keeps simulating until the pushed value has
  * propagated to the flag (so the round-trip is guaranteed to complete regardless of simulator
  * speed), then a short settle margin so the flag emission is read before the sim ends; a large
  * hard cap bounds the run if nothing ever arrives.
  */
class InteractivePatternSim extends RTDesign:
  val sw = interactive_ctrl("sw", 8, POLL_US = 1) // viewer-driven input, polled every 1us
  val led = interactive_flag("led", 8) // design-driven output

  // mirror the received control onto the flag, through one registered design cycle
  val mirror = Bits(8) <> VAR.REG init h"00"
  mirror.din := sw.value
  led.value <> mirror

  // settle counts cycles since the control arrived (mirror non-zero); cap is the hard timeout.
  val settle = UInt(16) <> VAR.REG init 0
  val cap = UInt(32) <> VAR.REG init 0
  cap.din := cap + 1
  if (mirror.uint != 0) settle.din := settle + 1
  else settle.din := settle
  if (settle == 2000 | cap == 5000000) finish()
end InteractivePatternSim

/** End-to-end test of the shared `interactive` foreign-IP bundle ([[interactive_ctrl]] +
  * [[interactive_flag]]) across every locally-installed simulator, per the bundle's supported
  * foreign-function interfaces:
  *   - verilog backend -> DPI (Verilator, Questa/vlog, Vivado XSim/xvlog) and VPI (Icarus)
  *   - vhdl backend -> VHPI (GHDL, NVC)
  *
  * Questa and Vivado XSim are covered through their DPI (verilog) path; the bundled VHPI shim
  * targets the GHDL/NVC VHPIDIRECT ABI, which they do not share.
  *
  * This also exercises the bundle's commonality: two distinct IPs share one C++ singleton backend,
  * one socket/viewer hook, one set of FFI libraries and (on the VHDL side) one
  * `interactive_pkg.vhdl` package — all of which the tool-execution stage must copy/link/compile
  * exactly once.
  *
  * Each tool drives [[InteractivePatternSim]] in headless capture mode: the hook pushes `sw=42` and
  * must read `led=42` back. Tools not on the local PATH are skipped (`assume`).
  */
class InteractiveSimSpec extends munit.FunSuite:
  // the slower vendor simulators (xsim/questa) compile + run well past munit's 30s default
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private val verilogSims: List[(String, VerilogSimulator)] = List(
    "verilator" -> simulators.verilator, // DPI
    "iverilog" -> simulators.iverilog, // VPI
    "questa" -> simulators.vlog, // DPI
    "xsim" -> simulators.xvlog // DPI
  )
  // VHPI only: the bundled VHDL shim is GHDL/NVC VHPIDIRECT (Questa/XSim are covered via DPI above).
  private val vhdlSims: List[(String, VHDLSimulator)] = List(
    "ghdl" -> simulators.ghdl,
    "nvc" -> simulators.nvc
  )

  // shared, tool-independent options
  given options.OnError = _.Exception // a tool error fails the test rather than exiting the JVM
  given SimulatorOptions.Location = _.local
  given SimulatorOptions.RunLimit = None
  given CompilerOptions.NewFolderForTop = false

  private val sendValue = 42L

  // Drives InteractivePatternSim through the interactive bundle in headless capture mode and asserts
  // the viewer read back exactly the value it pushed (the full round-trip).
  private def captureAndAssert(label: String)(using CompilerOptions, SimulatorOptions): Unit =
    val capture = os.temp(prefix = s"interactive_$label", suffix = ".txt")
    System.setProperty("dfhdl.ips.interactive.send", s"sw=$sendValue")
    System.setProperty("dfhdl.ips.interactive.capture", capture.toString)
    try
      (new InteractivePatternSim).compile.commit.simPrep.simRun
      // the viewer writes the capture file from a background thread joined at sim end; brief margin
      var waited = 0
      while (os.size(capture) == 0 && waited < 5000)
        Thread.sleep(100); waited += 100
      val flags = os.read.lines(capture).iterator
        .map(_.trim).filter(_.nonEmpty)
        .flatMap { line =>
          line.split("=", 2) match
            case Array(k, v) => v.trim.toLongOption.map(k.trim -> _)
            case _           => None
        }
        .toMap
      assert(flags.nonEmpty, s"[$label] no flag values were captured")
      assertEquals(
        flags.get("led"),
        Some(sendValue),
        s"[$label] viewer<->design round-trip mismatch (captured: $flags)"
      )
    finally
      System.clearProperty("dfhdl.ips.interactive.send")
      System.clearProperty("dfhdl.ips.interactive.capture")
      os.remove.all(capture)
    end try
  end captureAndAssert

  for ((name, sim) <- verilogSims)
    test(s"interactive end-to-end round-trip: verilog/$name") {
      assume(sim.isAvailable, s"$name not installed locally; skipping")
      val label = s"verilog-$name"
      given CompilerOptions.Backend = _.verilog
      given SimulatorOptions.VerilogSimulator = _ => sim
      given CompilerOptions.CommitFolder = s"sandbox${S}InteractiveSimSpec$S$label"
      captureAndAssert(label)
    }

  for ((name, sim) <- vhdlSims)
    test(s"interactive end-to-end round-trip: vhdl/$name") {
      assume(sim.isAvailable, s"$name not installed locally; skipping")
      val label = s"vhdl-$name"
      given CompilerOptions.Backend = _.vhdl
      given SimulatorOptions.VHDLSimulator = _ => sim
      given CompilerOptions.CommitFolder = s"sandbox${S}InteractiveSimSpec$S$label"
      captureAndAssert(label)
    }
end InteractiveSimSpec
