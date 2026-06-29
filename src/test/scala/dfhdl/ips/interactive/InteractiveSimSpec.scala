package dfhdl.ips.interactive

import dfhdl.*
import dfhdl.options.{CompilerOptions, SimulatorOptions}
import dfhdl.tools.simulators
import dfhdl.tools.toolsCore.{VerilogSimulator, VHDLSimulator}
import java.io.{BufferedReader, InputStreamReader}
import java.net.{InetAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets.UTF_8
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
  * one socket, one set of FFI libraries and (on the VHDL side) one `interactive_pkg.vhdl` package —
  * all of which the tool-execution stage must copy/link/compile exactly once.
  *
  * The actual viewer is `fpga-isv` (a GUI), so the round-trip is checked with a **pure socket
  * exchange**: the test stands in for the viewer/server, does the interactive-sim JSON handshake
  * directly (pushes `sw=42` when the control registers, reads back the `led` flag), and asserts the
  * value made the full loop. The hook honors `-Ddfhdl.ips.interactive.stream=host:port` to point
  * the backend at this test's server (and launch no GUI). Local only: the backend connects to
  * 127.0.0.1, which under dftools would be the sim container's loopback, not this host JVM —
  * fpga-isv itself works in dftools because it runs inside the hmi container, on the sim's WSL
  * network. Tools not on the local PATH are skipped (`assume`).
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

  // Drives InteractivePatternSim through the interactive bundle while THIS test plays the viewer over
  // a raw socket, and asserts the value pushed in came back out on the flag (the full round-trip).
  private def roundTripAndAssert(label: String)(using CompilerOptions, SimulatorOptions): Unit =
    val srv = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
    val port = srv.getLocalPort
    @volatile var led: Option[Long] = None
    val worker = new Thread(
      () =>
        try
          val sock = srv.accept()
          try exchange(sock, v => led = Some(v))
          finally sock.close()
        catch case _: Throwable => () // server closed at sim end, or peer reset - nothing to do
      ,
      s"interactive-test-viewer-$label"
    )
    worker.setDaemon(true)
    worker.start()
    // point the backend at this test's server and suppress the GUI viewer
    System.setProperty("dfhdl.ips.interactive.stream", s"127.0.0.1:$port")
    try
      (new InteractivePatternSim).compile.commit.simPrep.simRun
      worker.join(5000)
      assertEquals(
        led,
        Some(sendValue),
        s"[$label] viewer<->design round-trip mismatch (led=$led)"
      )
    finally
      System.clearProperty("dfhdl.ips.interactive.stream")
      try srv.close()
      catch case _: Throwable => ()
    end try
  end roundTripAndAssert

  // The interactive-sim JSON handshake from the viewer side: read newline-delimited messages; when
  // the "sw" control registers, push sw=42; record every "led" flag value the design emits.
  //   sim -> viewer  {"ev":"reg","t":..,"name":"sw","kind":"ctrl","width":8}
  //                  {"ev":"flag","t":..,"name":"led","val":42}
  //   viewer -> sim  {"name":"sw","val":42}
  private def exchange(sock: Socket, onLed: Long => Unit): Unit =
    val out = sock.getOutputStream
    val in = new BufferedReader(new InputStreamReader(sock.getInputStream, UTF_8))
    var line = in.readLine()
    while (line != null)
      field(line, "ev") match
        case Some("reg")
            if field(line, "kind").contains("ctrl") && field(line, "name").contains("sw") =>
          out.write((s"""{"name":"sw","val":$sendValue}""" + "\n").getBytes(UTF_8))
          out.flush()
        case Some("flag") if field(line, "name").contains("led") =>
          field(line, "val").flatMap(_.toLongOption).foreach(onLed)
        case _ => ()
      line = in.readLine()
  end exchange

  // minimal flat-JSON field extraction (string or bare/numeric token)
  private def field(line: String, key: String): Option[String] =
    val strRe =
      ("\"" + java.util.regex.Pattern.quote(key) + "\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\"").r
    strRe.findFirstMatchIn(line).map(_.group(1)).orElse {
      val numRe = ("\"" + java.util.regex.Pattern.quote(key) + "\"\\s*:\\s*([^,}\\s]+)").r
      numRe.findFirstMatchIn(line).map(_.group(1))
    }

  for ((name, sim) <- verilogSims)
    test(s"interactive end-to-end round-trip: verilog/$name") {
      assume(sim.isAvailable, s"$name not installed locally; skipping")
      val label = s"verilog-$name"
      given CompilerOptions.Backend = _.verilog
      given SimulatorOptions.VerilogSimulator = _ => sim
      given CompilerOptions.CommitFolder = s"sandbox${S}InteractiveSimSpec$S$label"
      roundTripAndAssert(label)
    }

  for ((name, sim) <- vhdlSims)
    test(s"interactive end-to-end round-trip: vhdl/$name") {
      assume(sim.isAvailable, s"$name not installed locally; skipping")
      val label = s"vhdl-$name"
      given CompilerOptions.Backend = _.vhdl
      given SimulatorOptions.VHDLSimulator = _ => sim
      given CompilerOptions.CommitFolder = s"sandbox${S}InteractiveSimSpec$S$label"
      roundTripAndAssert(label)
    }
end InteractiveSimSpec
