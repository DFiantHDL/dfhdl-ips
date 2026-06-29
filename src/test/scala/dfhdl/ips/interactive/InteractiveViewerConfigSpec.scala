package dfhdl.ips.interactive

/** Unit tests for [[InteractiveSimHook.resolveBoardArgs]]: the `<topName>.isv.json` auto-discovery
  * (classpath resources / current dir), its commit into the sim folder, and the fpga-isv board
  * arguments — all without launching the GUI or a simulator.
  */
class InteractiveViewerConfigSpec extends munit.FunSuite:
  private def ctx(
      topName: String,
      viewer: Option[String] = None,
      example: Option[String] = None
  ): InteractiveSimContext =
    // ipDir is unused by resolveBoardArgs (it takes execDir directly); streamOverride is irrelevant here
    new InteractiveSimContext("interactive", os.pwd, topName, None, viewer, example)

  test(
    "commits a <top>.isv.json found in the current working directory and points --config at it"
  ) {
    val top = "IsvCwdTop_unittest"
    val cwdCfg = os.pwd / s"$top.isv.json"
    val exec = os.temp.dir()
    val json = """{ "board": "from-cwd" }"""
    os.write.over(cwdCfg, json)
    try
      val args = InteractiveSimHook.resolveBoardArgs(ctx(top), exec, dftools = false)
      val committed = exec / s"$top.isv.json"
      assertEquals(args, Some(Seq("--config", committed.toString)))
      assert(os.exists(committed), "config was not committed to the sim folder")
      assertEquals(os.read(committed), json)
    finally
      os.remove.all(cwdCfg)
      os.remove.all(exec)
  }

  test("loads <top>.isv.json from the project resources (classpath) and commits it") {
    val exec = os.temp.dir() // resource file: ips/src/test/resources/IsvResTop.isv.json
    try
      val args = InteractiveSimHook.resolveBoardArgs(ctx("IsvResTop"), exec, dftools = false)
      val committed = exec / "IsvResTop.isv.json"
      assertEquals(args, Some(Seq("--config", committed.toString)))
      assert(os.read(committed).contains("from-classpath-resource"))
    finally os.remove.all(exec)
  }

  test("dftools: --config is relative to the mounted exec dir (the bare filename)") {
    val top = "IsvDftoolsTop_unittest"
    val cwdCfg = os.pwd / s"$top.isv.json"
    val exec = os.temp.dir()
    os.write.over(cwdCfg, "{}")
    try
      val args = InteractiveSimHook.resolveBoardArgs(ctx(top), exec, dftools = true)
      assertEquals(args, Some(Seq("--config", s"$top.isv.json")))
      assert(os.exists(exec / s"$top.isv.json"))
    finally
      os.remove.all(cwdCfg)
      os.remove.all(exec)
  }

  test("an explicit viewer-path override is used and committed under <top>.isv.json") {
    val exec = os.temp.dir()
    val explicit = os.temp(suffix = ".json")
    os.write.over(explicit, """{ "board": "explicit-override" }""")
    try
      val args =
        InteractiveSimHook.resolveBoardArgs(
          ctx("IsvAnyTop_unittest", viewer = Some(explicit.toString)),
          exec,
          dftools = false
        )
      val committed = exec / "IsvAnyTop_unittest.isv.json"
      assertEquals(args, Some(Seq("--config", committed.toString)))
      assert(os.read(committed).contains("explicit-override"))
    finally
      os.remove.all(exec)
      os.remove.all(explicit)
  }

  test("falls back to --example when no <top>.isv.json is found") {
    val exec = os.temp.dir()
    try
      assertEquals(
        InteractiveSimHook
          .resolveBoardArgs(ctx("IsvMissingTop_unittest", example = Some("ulx3s")), exec, false),
        Some(Seq("--example", "ulx3s"))
      )
    finally os.remove.all(exec)
  }

  test("returns None when no board is configured at all") {
    val exec = os.temp.dir()
    try assertEquals(
        InteractiveSimHook.resolveBoardArgs(ctx("IsvMissingTop_unittest"), exec, false),
        None
      )
    finally os.remove.all(exec)
  }
end InteractiveViewerConfigSpec
