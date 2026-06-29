package dfhdl.ips.video.vga

import dfhdl.*
import dfhdl.options.{CompilerOptions, SimulatorOptions}
import dfhdl.tools.simulators
import dfhdl.tools.toolsCore.{VerilogSimulator, VHDLSimulator, DFToolsImage}
import javax.imageio.ImageIO
import java.io.File.separatorChar as S

/** A self-checking VGA source: standard 640x480 timing driving the [[vga_monitor]] foreign IP with
  * an animated, scrolling colour gradient. The red/green channels are per-pixel gradients that
  * scroll with the frame counter (the per-pixel change gives the monitor fine transitions to lock
  * the dot clock onto), over a solid-blue floor so every active pixel is non-blank and the full
  * 640x480 area is detected.
  */
class VgaPatternSim extends RTDesign:
  val mon = vga_monitor()

  // 640x480 @ totals 800x525
  val hcount = UInt.until(800) <> VAR.REG init 0
  val vcount = UInt.until(525) <> VAR.REG init 0
  val frames = UInt(8) <> VAR.REG init 0

  if (hcount == 799)
    hcount.din := 0
    if (vcount == 524)
      vcount.din := 0
      frames.din := frames + 1
    else
      vcount.din := vcount + 1
      frames.din := frames
  else
    hcount.din := hcount + 1
    vcount.din := vcount
    frames.din := frames

  // combinational monitor drives
  val r = Bits(8) <> VAR
  val g = Bits(8) <> VAR
  val b = Bits(8) <> VAR
  val hs = Bit <> VAR
  val vs = Bit <> VAR

  hs := !((hcount >= 656) & (hcount < 752)) // H sync pulse (active low)
  vs := !((vcount >= 490) & (vcount < 492)) // V sync pulse (active low)

  val active = (hcount < 640) & (vcount < 480)
  r := active.sel(
    (hcount + frames).bits(7, 0),
    h"00"
  ) // horizontal gradient scrolling with the frame counter
  g := active.sel(
    (vcount + frames).bits(7, 0),
    h"00"
  ) // vertical gradient scrolling with the frame counter
  b := active.sel(h"ff", h"00") // solid-blue floor: every active pixel is non-blank

  mon.r <> r
  mon.g <> g
  mon.b <> b
  mon.hsync <> hs
  mon.vsync <> vs

  // run plenty of frames so the interactive demo shows the animation for a while; the capture test
  // exits much earlier (the backend stops after VGA_MONITOR_FRAMES, a few frames in), so this is
  // just a backstop
  if (frames == 60) finish()
end VgaPatternSim

/** End-to-end test of the `vga_monitor` foreign IP across every locally-installed simulator, per
  * the IP's supported foreign-function interfaces:
  *   - verilog backend → DPI (Verilator, Questa/vlog, Vivado XSim/xvlog) and VPI (Icarus)
  *   - vhdl backend → VHPI (GHDL, NVC)
  *
  * Questa and Vivado XSim are covered through their DPI (verilog) path: the bundled VHPI shim
  * targets the GHDL/NVC VHPIDIRECT ABI, which Questa (FLI) and XSim do not share, so the IP has no
  * VHDL-side shim for them.
  *
  * Each tool drives [[VgaPatternSim]] in headless capture mode and must let the viewer reconstruct
  * a full, non-blank 640x480 frame. Tools not on the local PATH are skipped (`assume`). Heavy
  * integration test: needs the tool plus the bundled per-FFI shim for the host platform.
  */
class VgaMonitorSimSpec extends munit.FunSuite:
  // raise the default per-test timeout: the slower vendor simulators (xsim/questa) compile + run well
  // past munit's 30s default
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
  // The dftools (image) cases run a representative subset to bound image pulls: verilator (DPI,
  // `sim-verilator`) and ghdl (VHPI, `sim-llvm`). The viewer runs from the `hmi` image either way.
  private val dftoolsVerilog = Set("verilator")
  private val dftoolsVhdl = Set("ghdl")
  // The DFTools image each subset simulator runs from (used to gate the dftools cases).
  private val simImage: Map[String, String] =
    Map("verilator" -> "sim-verilator", "ghdl" -> "sim-llvm")

  // shared, tool-independent options
  given options.OnError = _.Exception // a tool error fails the test rather than exiting the JVM
  given SimulatorOptions.RunLimit = None
  given CompilerOptions.NewFolderForTop = false

  // Is ffmpeg on the local PATH? Required by the local (host) capture cases; the dftools cases get it
  // from the `hmi` image instead.
  private lazy val localFfmpeg: Boolean =
    try os.proc("ffmpeg", "-hide_banner", "-version").call(check = false).exitCode == 0
    catch case _: Throwable => false

  // Drives VgaPatternSim into the vga_monitor IP in headless capture mode and asserts ffmpeg grabbed
  // a full, non-blank 640x480 frame. The capture PNG lives under the commit folder (the mounted cwd),
  // so it is host-readable whether ffmpeg ran locally or inside the `hmi` image.
  private def captureAndAssert(label: String, commitFolder: String)(using
      CompilerOptions,
      SimulatorOptions
  ): Unit =
    val capture = os.Path(commitFolder, os.pwd) / "vga_frame.png"
    os.remove.all(capture)
    System.setProperty("dfhdl.ips.vga_monitor.capture", capture.toString)
    System.setProperty("dfhdl.ips.vga_monitor.captureFrame", "0")
    try
      (new VgaPatternSim).compile.commit.simPrep.simRun
      // the grabber writes the PNG from a background thread joined at sim end; allow a brief margin
      var waited = 0
      while ((!os.exists(capture) || os.size(capture) == 0) && waited < 5000)
        Thread.sleep(100); waited += 100
      val img = if (os.exists(capture)) ImageIO.read(capture.toIO) else null
      assert(img != null, s"[$label] no frame image was captured")
      // the viewer carries no fixed size — these dimensions are recovered purely from the per-frame
      // PPM header (VGA_MONITOR_FORMAT=ppm), so matching them proves the metadata round-trip
      assertEquals(img.getWidth, 640, s"[$label] frame width")
      assertEquals(img.getHeight, 480, s"[$label] frame height")
      // a real captured frame is mostly non-blank (the pattern keeps the blue channel high)
      var bright = 0
      for (y <- 0 until img.getHeight; x <- 0 until img.getWidth)
        if ((img.getRGB(x, y) & 0xff) > 128) bright += 1
      assert(
        bright > img.getWidth * img.getHeight / 2,
        s"[$label] captured frame looks blank ($bright bright pixels)"
      )
    finally
      System.clearProperty("dfhdl.ips.vga_monitor.capture")
      System.clearProperty("dfhdl.ips.vga_monitor.captureFrame")
      os.remove.all(capture)
    end try
  end captureAndAssert

  // local (host PATH) and dftools (DFTools image) variants of the same capture matrix.
  private val locations: List[(String, Boolean)] = List("local" -> false, "dftools" -> true)

  for
    (locName, dftools) <- locations
    (name, sim) <- verilogSims
    if !dftools || dftoolsVerilog(name)
  do
    test(s"vga_monitor end-to-end frame capture: $locName/verilog/$name") {
      val label = s"$locName-verilog-$name"
      given CompilerOptions.Backend = _.verilog
      given SimulatorOptions.VerilogSimulator = _ => sim
      given SimulatorOptions.Location = if (dftools) _.dftools else _.local
      given CompilerOptions.CommitFolder = s"sandbox${S}VgaMonitorSimSpec$S$label"
      if (dftools)
        assume(DFToolsImage.isAvailable(simImage(name)), s"$name image unavailable; skipping")
        assume(DFToolsImage.isAvailable("hmi"), "hmi (ffmpeg) image unavailable; skipping")
      else
        assume(sim.isAvailable, s"$name not installed locally; skipping")
        assume(localFfmpeg, "ffmpeg not on local PATH; skipping")
      captureAndAssert(label, s"sandbox${S}VgaMonitorSimSpec$S$label")
    }
  end for

  for
    (locName, dftools) <- locations
    (name, sim) <- vhdlSims
    if !dftools || dftoolsVhdl(name)
  do
    test(s"vga_monitor end-to-end frame capture: $locName/vhdl/$name") {
      val label = s"$locName-vhdl-$name"
      given CompilerOptions.Backend = _.vhdl
      given SimulatorOptions.VHDLSimulator = _ => sim
      given SimulatorOptions.Location = if (dftools) _.dftools else _.local
      given CompilerOptions.CommitFolder = s"sandbox${S}VgaMonitorSimSpec$S$label"
      if (dftools)
        assume(DFToolsImage.isAvailable(simImage(name)), s"$name image unavailable; skipping")
        assume(DFToolsImage.isAvailable("hmi"), "hmi (ffmpeg) image unavailable; skipping")
      else
        assume(sim.isAvailable, s"$name not installed locally; skipping")
        assume(localFfmpeg, "ffmpeg not on local PATH; skipping")
      captureAndAssert(label, s"sandbox${S}VgaMonitorSimSpec$S$label")
    }
  end for
end VgaMonitorSimSpec
