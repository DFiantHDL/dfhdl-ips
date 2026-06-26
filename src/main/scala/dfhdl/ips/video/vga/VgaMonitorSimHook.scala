package dfhdl.ips.video.vga

import dfhdl.tools.{ForeignSimHook, ForeignSimContext}
import java.awt.image.BufferedImage
import java.io.{BufferedInputStream, InputStream}
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
  * the video geometry it connects to `VGA_MONITOR_STREAM=host:port` and streams finished frames.
  * This hook requests the v0.4.0 self-describing `VGA_MONITOR_FORMAT=ppm` format (see [[simEnv]]),
  * so each frame arrives as a concatenated binary PPM: a `P6\n<W> <H>\n255\n` ASCII header followed
  * by `W*H*3` raw RGB bytes. The hook listens on an ephemeral port, reads the per-frame header to
  * recover the resolution from the stream itself, and decodes frames into a [[BufferedImage]].
  *
  *   - interactive (default): shows a Swing window that repaints each frame
  *   - capture (test): runs headless and writes the chosen frame to the requested PNG, then asks
  *     the backend to stop (via `VGA_MONITOR_FRAMES`)
  *
  * The frame size is whatever the PPM headers report (no fixed default); a size change mid-stream
  * reallocates the image. A single monitor instance is supported.
  */
object VgaMonitorSimHook extends ForeignSimHook[VgaMonitorSimContext]:
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
  end context

  override def onSimStart(ctx: VgaMonitorSimContext): Unit =
    val srv = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
    ctx.server = srv
    ctx.port = srv.getLocalPort
    // the frame size is not known until the first PPM header arrives, so the image and the (optional)
    // window are created by the worker once it reads that header.
    val t = new Thread(
      () =>
        try
          val sock = srv.accept()
          try streamFrames(sock, ctx)
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
    // request the self-describing PPM stream (v0.4.0) so the viewer recovers each frame's geometry
    // from the per-frame P6 header instead of assuming a fixed size.
    val base = Map(
      "VGA_MONITOR_STREAM" -> s"127.0.0.1:${ctx.port}",
      "VGA_MONITOR_FORMAT" -> "ppm"
    )
    // In capture mode, bound the run with VGA_MONITOR_FRAMES so it terminates, but leave a margin
    // ABOVE the captured frame. The backend `exit(0)`s the instant it hits this limit; if that limit
    // were the captured frame itself, that abrupt exit races (and on a fast simulator like Verilator
    // resets) the socket while we're still reading the frame. With margin, the viewer grabs its
    // frame from a still-live stream and closes first (the backend then sees "viewer gone" and stops
    // cleanly), so the read always completes. Mirrors upstream's e2e, which streams ~12 frames while
    // the viewer grabs one.
    ctx.captureToOpt match
      case Some(_) => base + ("VGA_MONITOR_FRAMES" -> (ctx.captureFrame + 8).toString)
      case None    => base
  end simEnv

  override def onSimEnd(ctx: VgaMonitorSimContext): Unit =
    Option(ctx.worker).foreach(_.join(5000))
    Option(ctx.server).foreach(s =>
      try s.close()
      catch case _: Throwable => ()
    )
    // leave the interactive window open so the user can inspect the final frame; it closes via the
    // window's own DISPOSE_ON_CLOSE. (Capture mode is headless - there is no window to keep.)

  // Reads one whitespace-delimited PPM token (the magic, or a numeric field), skipping leading
  // whitespace and `#` comment lines, and consuming the single whitespace that terminates it (so the
  // one separator before the binary raster is eaten after the maxval). Returns "" at end of stream.
  private def readPpmToken(in: InputStream): String =
    var c = in.read()
    var skipping = true
    while (skipping)
      if (c < 0) skipping = false
      else if (c == '#') // comment: skip to end of line, then keep skipping whitespace
        while (c >= 0 && c != '\n') c = in.read()
      else if (Character.isWhitespace(c)) c = in.read()
      else skipping = false
    val sb = new StringBuilder
    while (c >= 0 && !Character.isWhitespace(c))
      sb.append(c.toChar)
      c = in.read()
    sb.toString
  end readPpmToken

  private def streamFrames(sock: Socket, ctx: VgaMonitorSimContext): Unit =
    val in = new BufferedInputStream(sock.getInputStream)
    var img: BufferedImage = null
    var buf: Array[Byte] = null
    var curW = -1
    var curH = -1
    var idx = 0
    var continue = true
    while (continue)
      // each frame is a binary PPM: "P6\n<W> <H>\n255\n" then W*H*3 raw RGB bytes
      val magic = readPpmToken(in)
      val w = readPpmToken(in).toIntOption.getOrElse(-1)
      val h = readPpmToken(in).toIntOption.getOrElse(-1)
      val maxv = readPpmToken(in).toIntOption.getOrElse(-1)
      if (magic != "P6" || w <= 0 || h <= 0 || maxv != 255)
        continue = false // end of stream or unexpected header
      else
        // (re)allocate the image/buffer/window on the first frame or whenever the size changes
        if (img == null || w != curW || h != curH)
          curW = w
          curH = h
          img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
          buf = new Array[Byte](w * h * 3)
          ensureWindow(ctx, img)
        val frameBytes = w * h * 3
        var off = 0
        var eof = false
        while (off < frameBytes && !eof)
          val n = in.read(buf, off, frameBytes - off)
          if (n < 0) eof = true else off += n
        if (off < frameBytes) continue = false // truncated final frame
        else
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
      end if
    end while
  end streamFrames

  // In interactive mode, (re)create the viewer window bound to the given image. Called when the image
  // is (re)allocated, i.e. on the first frame and on any mid-stream size change. No-op in capture mode.
  private def ensureWindow(ctx: VgaMonitorSimContext, img: BufferedImage): Unit =
    if (ctx.captureToOpt.isEmpty)
      SwingUtilities.invokeLater { () =>
        Option(ctx.frame).foreach(_.dispose())
        val f = new JFrame(s"VGA Monitor - ${ctx.topName}")
        f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        f.add(new JLabel(new ImageIcon(img)))
        f.pack()
        f.setVisible(true)
        ctx.frame = f
      }
  end ensureWindow
end VgaMonitorSimHook
