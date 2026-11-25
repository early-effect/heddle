package heddle.build

import histos.*
import histos.plugin.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.Base64
import _root_.scala.jdk.CollectionConverters.*
import zio.*
import zio.test.*

object RelayBrowserLiveSpec extends ZIOSpecDefault:
  private val files = new RelayBrowserFiles
  private val live  = new RelayBrowserLive(files)

  def spec = suite("live relay/browser tasks")(
    test("script delimiters, literal reconstruction and CSP hash obey the escaped UTF-8 byte law") {
      val input = Gen.stringBounded(0, 300)(Gen.char)
      check(input, Gen.elements("</script", "</ScRiPt", "</SCRIPT")): (prefix, delimiter) =>
        val linked = prefix + delimiter + "\n\\\"\u0000😀"
        RelaySource
          .generate(linked)
          .map: source =>
            assertTrue(
              !source.text.toLowerCase(java.util.Locale.ROOT).contains("</script"),
              source.text.replace("<\\/", "</") == linked,
              source.literals
                .map(decodeLiteral)
                .foldLeft(Option(""))((all, part) => for left <- all; right <- part yield left + right)
                .contains(source.text),
              source.hash == hash(source.text),
              source.scalaSource.contains(s"ScriptHash(\"${source.hash}\")"),
            )
    },
    test("chunk boundaries preserve controls, Unicode and supplementary characters under the constant limit") {
      check(Gen.elements(0, 8191, 8192, 8193, 14999, 15000, 30001)): size =>
        val linked = "\u0000" * size + "😀</script>\n\t\r\\\""
        RelaySource
          .generate(linked)
          .map: source =>
            val decoded = source.literals.flatMap(decodeLiteral).mkString
            assertTrue(
              decoded == source.text,
              source.literals.forall(_.getBytes(UTF_8).length < 65535),
              source.literals.forall(_.length <= 8192 * 6 + 2),
              source.hash == hash(decoded),
            )
    },
    test("source matches the protected host template, including empty and ordinary linked products") {
      val linked = "(()=>{const x='<\\/script>';console.log(x)})();\n"
      for
        source <- RelaySource.generate(linked)
        empty  <- RelaySource.generate("")
      yield assertTrue(
        source.scalaSource == expectedSource(source.hash, source.literals),
        source.text == linked,
        empty.text.isEmpty,
        empty.hash == hash(""),
        empty.scalaSource == expectedSource(empty.hash, Nil),
      )
    },
    test("relay consumes only the effective Full production product and writes its named staging destination") {
      ZIO.scoped:
        for
          root <- temporary
          full       = root.resolve("linked/full.js")
          fast       = root.resolve("linked/fast.js")
          out        = root.resolve("stage/exact.scala")
          production = "(()=>{const text='</script>';console.log(text)})();\n"
          _ <- files.write(full, production)
          _ <- files.write(fast, "FAST MUST NOT BE USED")
          req = request(TaskSpec.relay, root).copy(
            linked = List(link("mcpAppsRelay", fast, JsOptimize.Fast), link("mcpAppsRelay", full, JsOptimize.Full)),
            destinations = OutputDestinations(List(OutputDestination.File(OutputName("relay-source"), out))),
          )
          result    <- invoke(TaskSpec.relay, req)
          written   <- files.read(out)
          expected  <- RelaySource.generate(production)
          published <- ZIO
            .attemptBlocking(Files.exists(root.resolve("target/generated/relay/RelayScript.scala")))
            .mapError(_ => RelayBrowserError.Read(root))
        yield assertTrue(
          written == expected.scalaSource,
          !written.contains("FAST MUST NOT BE USED"),
          !published,
          result.files == List(
            WrittenFile(OutputName("relay-source"), RepoPath("target/generated/relay/RelayScript.scala"))
          ),
          result.directories.isEmpty,
        )
    },
    test("wrong module, version, mode and ambiguous Full products fail with the expected link ADT") {
      val path    = Path.of("never-read.js")
      val full    = link("mcpAppsRelay", path, JsOptimize.Full)
      val invalid = List(
        Nil,
        List(full.copy(module = ProjectName("other"))),
        List(full.copy(version = scala"3.9.0")),
        List(full.copy(optimize = JsOptimize.Fast)),
        List(full, full),
      )
      ZIO
        .foreach(invalid): linked =>
          live.execute(TaskSpec.relay, request(TaskSpec.relay, Path.of(".")).copy(linked = linked)).either
        .map: results =>
          val expected = ExpectedBrowserLink(ProjectName("mcpAppsRelay"), Catalog.scala3, JsOptimize.Full)
          assertTrue(
            results == List(0, 0, 0, 0, 2)
              .map(count => Left(RelayBrowserError.ExpectedNamedLinkFailure(expected, count)))
          )
    },
    test("missing or Directory staging destinations are typed failures and are never widened") {
      ZIO.scoped:
        for
          root <- temporary
          full = root.resolve("full.js")
          _ <- files.write(full, "console.log('relay');")
          req = request(TaskSpec.relay, root).copy(linked = List(link("mcpAppsRelay", full, JsOptimize.Full)))
          absent <- invoke(TaskSpec.relay, req).either
          wrong  <- invoke(
            TaskSpec.relay,
            req.copy(destinations =
              OutputDestinations(
                List(OutputDestination.Directory(OutputName("relay-source"), root.resolve("stage")))
              )
            ),
          ).either
        yield assertTrue(
          absent == Left(PluginRunError.MissingDestination(OutputName("relay-source"))),
          wrong == Left(
            PluginRunError.OutputKindMismatch(OutputName("relay-source"), OutputKind.File, OutputKind.Directory)
          ),
        )
    },
    test("Fast browser resources are two byte-exact named files, with unrelated siblings untouched") {
      ZIO.scoped:
        for
          root <- temporary
          view     = root.resolve("linked/view.js")
          host     = root.resolve("linked/host.js")
          viewOut  = root.resolve("stage/counter-view.js")
          hostOut  = root.resolve("stage/reference-host.js")
          sibling  = root.resolve("stage/keep.txt")
          viewText = "const view='😀';\r\n"
          hostText = "const host='</script>';\n"
          _ <- files.write(view, viewText)
          _ <- files.write(host, hostText)
          _ <- files.write(sibling, "keep")
          req = request(TaskSpec.browserAssets, root).copy(
            linked =
              List(link("appsBrowserView", view, JsOptimize.Fast), link("appsBrowserHost", host, JsOptimize.Fast)),
            destinations = OutputDestinations(
              List(
                OutputDestination.File(OutputName("counter-view"), viewOut),
                OutputDestination.File(OutputName("reference-host"), hostOut),
              )
            ),
          )
          result     <- invoke(TaskSpec.browserAssets, req)
          actualView <- bytes(viewOut)
          actualHost <- bytes(hostOut)
          keep       <- files.read(sibling)
        yield assertTrue(
          actualView == viewText.getBytes(UTF_8).toList,
          actualHost == hostText.getBytes(UTF_8).toList,
          keep == "keep",
          result.files == List(
            WrittenFile(OutputName("counter-view"), RepoPath("target/generated/browser/counter-view.js")),
            WrittenFile(OutputName("reference-host"), RepoPath("target/generated/browser/reference-host.js")),
          ),
          result.directories.isEmpty,
        )
    },
    test("browser check inspects the actual linked ES module's reachable chunks, not sibling files") {
      ZIO.scoped:
        for
          root <- temporary
          entry = root.resolve("main.js")
          chunk = root.resolve("chunk.js")
          _ <- files.write(entry, "import{run}from'./chunk.js';run();")
          _ <- files.write(chunk, "export function run(){}; export * from './main.js';")
          _ <- files.write(root.resolve("unreachable.js"), "import 'node:net';")
          req = request(TaskSpec.browserCheck, root).copy(linked = List(link("browser", entry, JsOptimize.Full)))
          clean    <- invoke(TaskSpec.browserCheck, req)
          _        <- files.write(chunk, "import{readFile}from'node:fs/promises';export function run(){readFile('x')}")
          rejected <- live.execute(TaskSpec.browserCheck, req).either
        yield assertTrue(
          clean.files.isEmpty,
          clean.directories.isEmpty,
          rejected == Left(RelayBrowserError.NodeImports(chunk.toAbsolutePath.normalize, List("node:fs/promises"))),
        )
    },
    test("Node specifiers in static, side-effect, dynamic, re-export and escaped imports are recognized") {
      val code =
        """import{a}from"node:fs";import'net';import("tls");export{a}from"crypto";import"\u006eode:os";import(`node:path`);"""
      val decoys = """// import 'node:fs'
        |/* export * from 'net' */ const text="import 'tls'";const template=`import 'node:crypto'`;
        |""".stripMargin
      assertTrue(
        BrowserImports.modules(code) == List("node:fs", "net", "tls", "crypto", "node:os", "node:path"),
        BrowserImports.modules(decoys).isEmpty,
        BrowserImports.isNode("fs/promises"),
        !BrowserImports.isNode("./fs.js"),
      )
    },
    test("missing linked chunks are typed I/O failures rather than a vacuous Node-free success") {
      ZIO.scoped:
        for
          root <- temporary
          entry   = root.resolve("main.js")
          missing = root.resolve("missing.js")
          _       <- files.write(entry, "export * from './missing.js';")
          checked <- files.checkBrowser(entry).either
        yield assertTrue(checked == Left(RelayBrowserError.Read(missing.toAbsolutePath.normalize)))
    },
    test("large minified linked modules do not consume the call stack") {
      val code = "const x=1;" * 100000 + "import 'node:crypto';"
      assertTrue(BrowserImports.modules(code) == List("node:crypto"))
    },
  )

  private def invoke(task: TaskSpec, request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    val run = ZIO.serviceWithZIO[RelayBrowserTasks](_.run(task, request))
    ZIO.scopeWith: scope =>
      run.provideSomeLayer[Scope](RelayBrowserLive.layer ++ PluginScopes.live(scope))

  private def request(task: TaskSpec, root: Path): RunRequest = RunRequest(
    PluginTaskId(TaskSpec.pluginId, task.name),
    root,
    Nil,
    PluginArgs.empty,
    Nil,
    InvocationContext(
      InvocationId.derive("relay-browser-tests"),
      ClientId("relay-browser-tests"),
      CommandIdentity.Task(PluginTaskId(TaskSpec.pluginId, task.name)),
      histos.lock.ExecutionKind.Local,
      Interaction.Unattended,
      OfflineSelection.Offline,
    ),
    PluginEvents.closedInvocation,
  )

  private inline def link(inline module: String, path: Path, optimize: JsOptimize): LinkedScript =
    LinkedScript(ProjectName(module), Catalog.scala3, path, optimize)

  private def bytes(path: Path): IO[RelayBrowserError, List[Byte]] =
    ZIO.attemptBlocking(Files.readAllBytes(path).toList).mapError(_ => RelayBrowserError.Read(path))

  private def temporary: ZIO[Scope, RelayBrowserError, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking(Files.createTempDirectory("heddle-relay-browser-"))
        .mapError(_ => RelayBrowserError.Write(Path.of(".")))
    ): root =>
      ZIO.attemptBlocking {
        val walk = Files.walk(root)
        try walk.iterator.asScala.toList.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
        finally walk.close()
      }.ignore

  private def hash(text: String): String =
    "sha256-" + Base64.getEncoder.encodeToString(MessageDigest.getInstance("SHA-256").digest(text.getBytes(UTF_8)))

  private def decodeLiteral(literal: String): Option[String] =
    @ _root_.scala.annotation.tailrec
    def loop(chars: List[Char], result: List[Char]): Option[String] = chars match
      case '\\' :: 'u' :: a :: b :: c :: d :: rest =>
        val digits = List(a, b, c, d).map(Character.digit(_, 16))
        if digits.forall(_ >= 0) then loop(rest, digits.foldLeft(0)(_ * 16 + _).toChar :: result) else None
      case '\\' :: c :: rest =>
        val decoded = c match
          case 'n'  => Some('\n')
          case 'r'  => Some('\r')
          case 't'  => Some('\t')
          case '\\' => Some('\\')
          case '"'  => Some('"')
          case _    => None
        decoded match
          case Some(value) => loop(rest, value :: result)
          case None        => None
      case c :: rest => loop(rest, c :: result)
      case Nil       => Some(result.reverse.mkString)
    if literal.startsWith("\"") && literal.endsWith("\"") then loop(literal.drop(1).dropRight(1).toList, Nil) else None

  private def expectedSource(hash: String, literals: List[String]): String =
    val parts = literals.mkString(",\n    ")
    s"""package heddle.mcp.apps.host
       |
       |import heddle.mcp.apps.ScriptHash
       |
       |/** The relay's linked script (`heddle-mcp-apps-relay`) and its CSP hash, generated by the build. Do not edit. */
       |private[host] object RelayScript:
       |  val hash: ScriptHash = ScriptHash("$hash")
       |
       |  val text: String = List(
       |    $parts
       |  ).mkString
       |""".stripMargin
end RelayBrowserLiveSpec
