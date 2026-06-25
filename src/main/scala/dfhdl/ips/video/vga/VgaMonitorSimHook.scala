package dfhdl.ips.video.vga

import dfhdl.tools.{ForeignSimHook, ForeignSimContext}
import java.awt.image.BufferedImage
import java.io.BufferedInputStream
import java.net.{InetAddress, ServerSocket, Socket}
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JFrame, JLabel, SwingUtilities, WindowConstants}

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
  // per-run viewer state, populated during onSimStart
  var server: ServerSocket = compiletime.uninitialized
  var worker: Thread = compiletime.uninitialized
  var frame: JFrame = compiletime.uninitialized
  var port: Int = -1

/** Simulation hook for the `vga_monitor` foreign IP.
  *
  * The vga-monitor backend, loaded into the simulator, acts as a TCP client: after it auto-locks
  * the video geometry it connects to `VGA_MONITOR_STREAM=host:port` and writes each finished frame
  * as raw RGB24 (`W*H*3` bytes, R,G,B order, no header). This hook therefore listens on an
  * ephemeral port (reported back via [[simEnv]]) and decodes frames into a [[BufferedImage]].
  *
  *   - interactive (default): shows a Swing window that repaints each frame
  *   - capture (test): runs headless and writes the chosen frame to the requested PNG, then asks
  *     the backend to stop (via `VGA_MONITOR_FRAMES`)
  *
  * Because the stream carries no dimensions, the frame size defaults to 640x480 and can be
  * overridden with `-Ddfhdl.ips.vga_monitor.size=WxH`. A single monitor instance is supported.
  */
object VgaMonitorSimHook extends ForeignSimHook[VgaMonitorSimContext]:
  private def frameSize: (Int, Int) =
    System.getProperty("dfhdl.ips.vga_monitor.size", "640x480").split("x") match
      case Array(w, h) => (w.trim.toInt, h.trim.toInt)
      case _           => (640, 480)

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

  override def onSimStart(ctx: VgaMonitorSimContext): Unit =
    val (w, h) = frameSize
    val srv = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
    ctx.server = srv
    ctx.port = srv.getLocalPort
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    if (ctx.captureToOpt.isEmpty)
      SwingUtilities.invokeLater { () =>
        val f = new JFrame(s"VGA Monitor - ${ctx.topName}")
        f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        f.add(new JLabel(new ImageIcon(img)))
        f.pack()
        f.setVisible(true)
        ctx.frame = f
      }
    val t = new Thread(
      () =>
        try
          val sock = srv.accept()
          try streamFrames(sock, img, w, h, ctx)
          finally sock.close()
        catch case _: Throwable => () // server closed at sim end, or peer reset - nothing to do
      ,
      "vga-monitor-viewer"
    )
    t.setDaemon(true)
    t.start()
    ctx.worker = t
  end onSimStart

  override def simEnv(ctx: VgaMonitorSimContext): Map[String, String] =
    val base = Map("VGA_MONITOR_STREAM" -> s"127.0.0.1:${ctx.port}")
    // in capture mode, ask the backend to stop after enough frames so the simulation terminates
    ctx.captureToOpt match
      case Some(_) => base + ("VGA_MONITOR_FRAMES" -> (ctx.captureFrame + 1).toString)
      case None    => base

  override def onSimEnd(ctx: VgaMonitorSimContext): Unit =
    Option(ctx.worker).foreach(_.join(5000))
    Option(ctx.server).foreach(s =>
      try s.close()
      catch case _: Throwable => ()
    )
    // leave the interactive window open so the user can inspect the final frame; it closes via the
    // window's own DISPOSE_ON_CLOSE. (Capture mode is headless - there is no window to keep.)

  private def streamFrames(
      sock: Socket,
      img: BufferedImage,
      w: Int,
      h: Int,
      ctx: VgaMonitorSimContext
  ): Unit =
    val in = new BufferedInputStream(sock.getInputStream)
    val frameBytes = w * h * 3
    val buf = new Array[Byte](frameBytes)
    var idx = 0
    var continue = true
    while (continue)
      // read exactly one frame (the stream has no framing/header - just W*H*3 raw RGB bytes)
      var off = 0
      var eof = false
      while (off < frameBytes && !eof)
        val n = in.read(buf, off, frameBytes - off)
        if (n < 0) eof = true else off += n
      if (off < frameBytes) continue = false
      if (continue)
        var p = 0
        var y = 0
        while (y < h)
          var x = 0
          while (x < w)
            val r = buf(p) & 0xff
            val g = buf(p + 1) & 0xff
            val b = buf(p + 2) & 0xff
            img.setRGB(x, y, (r << 16) | (g << 8) | b)
            p += 3
            x += 1
          y += 1
        Option(ctx.frame).foreach(f => SwingUtilities.invokeLater(() => f.repaint()))
        ctx.captureToOpt match
          case Some(path) if idx == ctx.captureFrame =>
            os.makeDir.all(path / os.up)
            ImageIO.write(img, "png", path.toIO)
            continue = false
          case _ =>
        idx += 1
      end if
    end while
  end streamFrames
end VgaMonitorSimHook
