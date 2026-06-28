package dfhdl.ips.interactive

/** Shared metadata for the `interactive` foreign-IP bundle (both [[interactive_ctrl]] and
  * [[interactive_flag]]).
  */
object InteractiveSim:
  /** The wrapped interactive-sim release, set from `build.sbt` (`interactiveSimVersion`) via the
    * generated `interactive.properties` resource (mirrors vga-monitor's `vga-monitor.properties`).
    * Bump it in `build.sbt`.
    */
  val version: String =
    val props = new java.util.Properties()
    // close the stream: a leaked handle blocks the build from re-copying the resource on Windows
    // (AccessDenied during copyResources).
    val inputStream = getClass.getClassLoader.getResourceAsStream("interactive.properties")
    try props.load(inputStream)
    finally inputStream.close()
    props.getProperty("interactive.version")
end InteractiveSim
