package dfhdl.ips.video.vga

import dfhdl.*
import dfhdl.options.{CompilerOptions, SimulatorOptions}
import javax.imageio.ImageIO

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
  if (active)
    r := (hcount + frames).bits(7, 0) // horizontal gradient scrolling with the frame counter
    g := (vcount + frames).bits(7, 0) // vertical gradient scrolling with the frame counter
    b := h"ff" // solid-blue floor: every active pixel is non-blank
  else
    r := h"00"; g := h"00"; b := h"00"

  mon.r <> r
  mon.g <> g
  mon.b <> b
  mon.hsync <> hs
  mon.vsync <> vs

  // run plenty of frames so the interactive demo shows the animation for a while; the capture test
  // exits much earlier (the backend stops after VGA_MONITOR_FRAMES=1), so this is just a backstop
  if (frames == 60) finish()
end VgaPatternSim

class VgaMonitorSimSpec extends munit.FunSuite:
  test("vga_monitor streams a frame the viewer captures (iverilog/VPI)") {
    // heavy integration test: needs a local iverilog and the bundled VPI shim - skip otherwise
    assume(
      dfhdl.internals.programIsAccessible("iverilog"),
      "iverilog not found on PATH; skipping vga_monitor end-to-end test"
    )
    given CompilerOptions.Backend = _.verilog
    given SimulatorOptions.VerilogSimulator = _.iverilog
    given SimulatorOptions.Location = _.local
    given SimulatorOptions.RunLimit = None

    val capture = os.temp(prefix = "vga_frame", suffix = ".png")
    System.setProperty("dfhdl.ips.vga_monitor.capture", capture.toString)
    System.setProperty("dfhdl.ips.vga_monitor.captureFrame", "0")
    try
      (new VgaPatternSim).compile.commit.simPrep.simRun

      // the viewer writes the PNG from a background thread joined at sim end; allow a brief margin
      var waited = 0
      while (os.size(capture) == 0 && waited < 3000)
        Thread.sleep(100); waited += 100
      val img = ImageIO.read(capture.toIO)
      assert(img != null, "no frame image was captured")
      assertEquals(img.getWidth, 640)
      assertEquals(img.getHeight, 480)
      // a real captured frame is mostly non-blank (the pattern keeps the blue channel high)
      var bright = 0
      for (y <- 0 until img.getHeight; x <- 0 until img.getWidth)
        if ((img.getRGB(x, y) & 0xff) > 128) bright += 1
      assert(
        bright > img.getWidth * img.getHeight / 2,
        s"captured frame looks blank ($bright bright pixels)"
      )
    finally
      System.clearProperty("dfhdl.ips.vga_monitor.capture")
      System.clearProperty("dfhdl.ips.vga_monitor.captureFrame")
      os.remove(capture)
    end try
  }
end VgaMonitorSimSpec
