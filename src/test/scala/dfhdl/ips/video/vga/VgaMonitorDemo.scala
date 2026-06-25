package dfhdl.ips.video.vga

import dfhdl.*
import dfhdl.options.{CompilerOptions, SimulatorOptions}

/** Runs [[VgaPatternSim]] *interactively* (no capture) so the VGA monitor viewer window pops up and
  * shows the reconstructed frames live. Needs a local `iverilog` on PATH.
  *
  * Run it with:
  * {{{
  *   sbtn.bat "ips/Test/runMain dfhdl.ips.video.vga.VgaMonitorDemo"
  * }}}
  *
  * The window opens while the simulation streams frames and stays up afterwards; close it to
  * finish.
  */
object VgaMonitorDemo:
  def main(args: Array[String]): Unit =
    given CompilerOptions.Backend = _.verilog
    given SimulatorOptions.VerilogSimulator = _.iverilog
    given SimulatorOptions.Location = _.local
    given SimulatorOptions.RunLimit = None
    (new VgaPatternSim).compile.commit.simPrep.simRun
    println("[VgaMonitorDemo] simulation finished - close the VGA Monitor window to exit.")
end VgaMonitorDemo
