package dfhdl.ips.interactive

import dfhdl.tools.{ForeignSimHook, ForeignSimContext}
import java.io.{BufferedReader, InputStreamReader, OutputStream}
import java.net.{InetAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable
import java.awt.{BorderLayout, Dimension, GridLayout}
import javax.swing.{
  BoxLayout, JButton, JFrame, JLabel, JPanel, JScrollPane, JTextField, SwingConstants,
  SwingUtilities, WindowConstants
}

/** The `interactive` IP-specific simulation context: the capture config (read from system
  * properties) plus the per-run viewer state. Shared by [[interactive_ctrl]] and
  * [[interactive_flag]] — one process-global backend, one socket, one viewer — so a single context
  * tracks every channel regardless of which IP it came from.
  */
final class InteractiveSimContext(
    ipName: String,
    ipDir: os.Path,
    topName: String,
    // when set, run headless and write the final flag values to this file (the e2e test path)
    val captureToOpt: Option[os.Path],
    // control values to push to the design, by channel NAME (capture/scripted mode)
    val sendValues: Map[String, Long]
) extends ForeignSimContext(ipName, ipDir, topName):
  // per-run viewer state, populated during onSimStart / the worker thread
  var server: ServerSocket = compiletime.uninitialized
  var worker: Thread = compiletime.uninitialized
  var frame: JFrame = compiletime.uninitialized
  var out: OutputStream = compiletime.uninitialized
  var port: Int = -1
  // latest value received per flag channel (insertion-ordered); guarded by `lock`
  val flagValues: mutable.LinkedHashMap[String, Long] = mutable.LinkedHashMap.empty
  // interactive-mode widgets, touched only on the Swing EDT
  val controlPanel: JPanel = new JPanel()
  val flagLabels: mutable.Map[String, JLabel] = mutable.Map.empty
  val lock = new Object
end InteractiveSimContext

/** Simulation hook for the `interactive` foreign-IP bundle ([[interactive_ctrl]] +
  * [[interactive_flag]]).
  *
  * The interactive-sim backend, loaded into the simulator, acts as a TCP client: each component
  * connects to `INTERACTIVE_STREAM=host:port` and exchanges newline-delimited JSON. This hook is
  * the server/viewer:
  *
  *   - sim -> viewer registration: `{"ev":"reg","name":"sw","kind":"ctrl","width":8}`
  *   - sim -> viewer flag value: `{"ev":"flag","t":<us>,"name":"led","val":42}`
  *   - sim -> viewer close: `{"ev":"close","name":"sw"}`
  *   - viewer -> sim control: `{"name":"sw","val":42}`
  *
  * Two modes:
  *   - interactive (default): a Swing window with a text field per control (Enter sends the value)
  *     and a live label per flag.
  *   - capture (test): headless. Pushes the configured control values
  *     (`-Ddfhdl.ips.interactive.send=sw=42;...`) as each control registers, records the latest
  *     value of every flag, and on sim end writes them to `-Ddfhdl.ips.interactive.capture` (one
  *     `name=value` line each) for the test to assert the viewer<->design round-trip.
  *
  * One hook instance serves the whole simulation (the tools layer dedups it per hook class), so it
  * handles every `interactive_ctrl`/`interactive_flag` channel over the one socket.
  */
object InteractiveSimHook extends ForeignSimHook[InteractiveSimContext]:
  private val sysPropPrefix = "dfhdl.ips.interactive"

  def context(base: ForeignSimContext): InteractiveSimContext =
    val captureToOpt =
      Option(System.getProperty(s"$sysPropPrefix.capture"))
        .filter(_.nonEmpty)
        .map(os.Path(_, os.pwd))
    val sendValues =
      parseSendValues(Option(System.getProperty(s"$sysPropPrefix.send")).getOrElse(""))
    new InteractiveSimContext(base.ipName, base.ipDir, base.topName, captureToOpt, sendValues)
  end context

  // parse "name=val;name=val" (also tolerates commas) into a NAME -> value map
  private def parseSendValues(spec: String): Map[String, Long] =
    spec.split("[;,]").iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { kv =>
        kv.split("=", 2) match
          case Array(k, v) => v.trim.toLongOption.map(k.trim -> _)
          case _           => None
      }
      .toMap

  override def onSimStart(ctx: InteractiveSimContext)(using
      dfhdl.options.SimulatorOptions
  ): Unit =
    val srv = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
    ctx.server = srv
    ctx.port = srv.getLocalPort
    val t = new Thread(
      () =>
        try
          val sock = srv.accept()
          try serve(sock, ctx)
          finally sock.close()
        catch case _: Throwable => () // server closed at sim end, or peer reset - nothing to do
      ,
      "interactive-viewer"
    )
    t.setDaemon(true)
    t.start()
    ctx.worker = t
  end onSimStart

  override def simEnv(ctx: InteractiveSimContext): Map[String, String] =
    Map("INTERACTIVE_STREAM" -> s"127.0.0.1:${ctx.port}")

  override def onSimEnd(ctx: InteractiveSimContext)(using dfhdl.options.SimulatorOptions): Unit =
    Option(ctx.worker).foreach(_.join(5000))
    Option(ctx.server).foreach(s =>
      try s.close()
      catch case _: Throwable => ()
    )
    // capture mode: persist the final flag values for the test to assert
    ctx.captureToOpt.foreach { path =>
      val content = ctx.lock.synchronized {
        ctx.flagValues.map((k, v) => s"$k=$v").mkString("\n")
      }
      os.makeDir.all(path / os.up)
      os.write.over(path, content)
    }
    // interactive mode: leave the window open so the user can inspect the final state; it closes via
    // the window's own DISPOSE_ON_CLOSE.
  end onSimEnd

  // Serve the one backend connection: read newline-delimited JSON and react. Holds the socket's
  // output stream so control values can be pushed back (on registration in capture mode, or on user
  // input in interactive mode).
  private def serve(sock: Socket, ctx: InteractiveSimContext): Unit =
    ctx.out = sock.getOutputStream
    val in = new BufferedReader(new InputStreamReader(sock.getInputStream, UTF_8))
    var line = in.readLine()
    while (line != null)
      handleLine(line, ctx)
      line = in.readLine()
  end serve

  private def handleLine(line: String, ctx: InteractiveSimContext): Unit =
    field(line, "ev") match
      case Some("reg") =>
        val name = field(line, "name").getOrElse("")
        val kind = field(line, "kind").getOrElse("")
        if (kind == "ctrl" && name.nonEmpty)
          // capture mode: push the configured value as soon as the control exists
          ctx.sendValues.get(name).foreach(v => send(ctx, name, v))
          if (ctx.captureToOpt.isEmpty) addControl(ctx, name)
        else if (kind == "flag" && name.nonEmpty && ctx.captureToOpt.isEmpty) addFlag(ctx, name)
      case Some("flag") =>
        val name = field(line, "name").getOrElse("")
        val v = field(line, "val").flatMap(_.toLongOption).getOrElse(0L)
        if (name.nonEmpty)
          ctx.lock.synchronized { ctx.flagValues(name) = v }
          if (ctx.captureToOpt.isEmpty) updateFlag(ctx, name, v)
      case _ => () // "close" and anything else: nothing to do
  end handleLine

  private def send(ctx: InteractiveSimContext, name: String, value: Long): Unit =
    val out = ctx.out
    if (out != null)
      val msg = s"""{"name":"$name","val":$value}""" + "\n"
      out.synchronized {
        out.write(msg.getBytes(UTF_8))
        out.flush()
      }
  end send

  // --- minimal JSON field extraction (the wire messages are flat objects) -----------------------
  // Returns the value of a quoted string field, or the raw token of a numeric/bare field, if present.
  private def field(line: String, key: String): Option[String] =
    val strRe =
      ("\"" + java.util.regex.Pattern.quote(key) + "\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\"").r
    strRe.findFirstMatchIn(line).map(_.group(1)).orElse {
      val numRe = ("\"" + java.util.regex.Pattern.quote(key) + "\"\\s*:\\s*([^,}\\s]+)").r
      numRe.findFirstMatchIn(line).map(_.group(1))
    }

  // --- interactive Swing viewer (no-op in capture mode) -----------------------------------------
  private def ensureFrame(ctx: InteractiveSimContext): Unit =
    if (ctx.frame == null)
      val f = new JFrame(s"Interactive - ${ctx.topName}")
      f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      ctx.controlPanel.setLayout(new BoxLayout(ctx.controlPanel, BoxLayout.Y_AXIS))
      val root = new JPanel(new BorderLayout())
      root.add(new JScrollPane(ctx.controlPanel), BorderLayout.CENTER)
      f.setContentPane(root)
      f.setPreferredSize(new Dimension(360, 240))
      f.pack()
      f.setVisible(true)
      ctx.frame = f

  private def addControl(ctx: InteractiveSimContext, name: String): Unit =
    SwingUtilities.invokeLater { () =>
      ensureFrame(ctx)
      val row = new JPanel(new GridLayout(1, 3, 4, 0))
      row.add(new JLabel(s"ctrl $name"))
      val tf = new JTextField("0")
      val btn = new JButton("set")
      def push(): Unit = tf.getText.trim.toLongOption.foreach(v => send(ctx, name, v))
      btn.addActionListener(_ => push())
      tf.addActionListener(_ => push())
      row.add(tf)
      row.add(btn)
      ctx.controlPanel.add(row)
      ctx.controlPanel.revalidate()
    }

  private def addFlag(ctx: InteractiveSimContext, name: String): Unit =
    SwingUtilities.invokeLater { () =>
      ensureFrame(ctx)
      if (!ctx.flagLabels.contains(name))
        val row = new JPanel(new GridLayout(1, 2, 4, 0))
        row.add(new JLabel(s"flag $name"))
        val value = new JLabel("-", SwingConstants.RIGHT)
        ctx.flagLabels(name) = value
        row.add(value)
        ctx.controlPanel.add(row)
        ctx.controlPanel.revalidate()
    }

  private def updateFlag(ctx: InteractiveSimContext, name: String, v: Long): Unit =
    SwingUtilities.invokeLater { () =>
      ensureFrame(ctx)
      ctx.flagLabels.get(name) match
        case Some(label) => label.setText(v.toString)
        case None        =>
          addFlag(ctx, name)
          ctx.flagLabels.get(name).foreach(_.setText(v.toString))
    }
end InteractiveSimHook
