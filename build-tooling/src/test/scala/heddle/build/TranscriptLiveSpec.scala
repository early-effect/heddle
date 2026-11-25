package heddle.build

import histos.*
import histos.plugin.*
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}
import _root_.scala.jdk.CollectionConverters.*

object TranscriptLiveSpec extends ZIOSpecDefault:
  private val runner = new TranscriptLive

  private val tests = suite("transcript invocation")(
    test("the real child receives the Main classpath and exact staged file argument") {
      ZIO.scoped:
        for
          repo <- temporary
          cp   <- classpath
          stage = repo.resolve("invocation/HarnessTranscript.scala")
          scope  <- ZIO.scope
          live   <- ZIO.service[DocsTranscriptTask].provideLayer(TranscriptLive.layer)
          result <- live
            .run(TaskSpec.transcript, request(repo, stage, cp))
            .provideSomeLayer[Scope](PluginScopes.live(scope))
          text <- ZIO.attemptBlockingIO(Files.readString(stage))
        yield assertTrue(
          text == TranscriptFixture.content,
          result.files == List(WrittenFile(TranscriptLive.outputName, TranscriptLive.outputPath)),
          result.files == TaskSpec.transcript.outputs.collect { case DeclaredOutput.File(name, path) =>
            WrittenFile(name, path)
          },
          result.directories.isEmpty,
          !Files.exists(repo.resolve(TranscriptLive.outputPath.value)),
        )
    },
    test("destination errors survive the plugin boundary") {
      ZIO.scoped:
        for
          repo  <- temporary
          scope <- ZIO.scope
          absent = request(repo, repo.resolve("stage"), Nil).copy(destinations = OutputDestinations.empty)
          error <- runner.run(TaskSpec.transcript, absent).flip.provideSomeLayer[Scope](PluginScopes.live(scope))
          wrongKind = absent.copy(destinations =
            OutputDestinations(
              List(
                OutputDestination.Directory(TranscriptLive.outputName, repo.resolve("stage"))
              )
            )
          )
          mismatch <- runner.run(TaskSpec.transcript, wrongKind).flip.provideSomeLayer[Scope](PluginScopes.live(scope))
        yield assertTrue(
          error == PluginRunError.MissingDestination(TranscriptLive.outputName),
          mismatch == PluginRunError.OutputKindMismatch(
            TranscriptLive.outputName,
            OutputKind.File,
            OutputKind.Directory,
          ),
        )
    },
    test("Test-complete, JS, and unrelated classpaths cannot substitute for docs JVM Main") {
      ZIO.scoped:
        for
          repo <- temporary
          cp   <- classpath
          original = request(repo, repo.resolve("stage"), cp)
          cells    = List(
            CellClasspath.testComplete(ProjectName("docs"), ScalaVersion("3.10.1"), Jvm, cp, cp),
            CellClasspath.mainOnly(ProjectName("docs"), ScalaVersion("3.10.1"), Js, cp),
            CellClasspath.mainOnly(ProjectName("heddle"), ScalaVersion("3.10.1"), Jvm, cp),
          )
          error <- runner.produce(original.copy(compiled = cells)).flip
        yield assertTrue(error == TranscriptError.MissingMainClasspath)
    },
    test("multiple Main cells and an empty Main classpath are refused") {
      ZIO.scoped:
        for
          repo <- temporary
          cp   <- classpath
          original = request(repo, repo.resolve("stage"), cp)
          ambiguous <- runner.produce(original.copy(compiled = original.compiled ++ original.compiled)).flip
          empty     <- runner.produce(request(repo, repo.resolve("stage"), Nil)).flip
        yield assertTrue(
          ambiguous == TranscriptError.AmbiguousMainClasspath,
          empty == TranscriptError.EmptyMainClasspath,
        )
    },
    test("a nonzero producer exit is a typed program failure") {
      ZIO.scoped:
        for
          repo  <- temporary
          error <- runner.produce(request(repo, repo.resolve("stage"), List(repo))).flip
        yield assertTrue(error == TranscriptError.ProgramFailed(1))
    },
    test("a child exiting zero without writing the destination is not success") {
      ZIO.scoped:
        for
          repo <- temporary
          cp   <- classpath
          _    <- ZIO.attemptBlockingIO(Files.writeString(repo.resolve("omit-output"), ""))
          stage = repo.resolve("stage")
          error <- runner.produce(request(repo, stage, cp)).flip
        yield assertTrue(error == TranscriptError.MissingFile(stage))
    },
    test("malformed runner configuration fails before launching the child") {
      ZIO.scoped:
        for
          repo  <- temporary
          cp    <- classpath
          error <- ZIO
            .withConfigProvider(ConfigProvider.fromMap(Map("java.home" -> "bad\u0000home")))(
              runner.produce(request(repo, repo.resolve("stage"), cp))
            )
            .flip
        yield assertTrue(error match
          case TranscriptError.Settings(_) => true
          case _                           => false)
    },
    test("staging I/O failures remain distinct from child failures") {
      ZIO.scoped:
        for
          repo <- temporary
          cp   <- classpath
          parent = repo.resolve("not-a-directory")
          _     <- ZIO.attemptBlockingIO(Files.writeString(parent, "occupied"))
          error <- runner.produce(request(repo, parent.resolve("stage"), cp)).flip
        yield assertTrue(error match
          case TranscriptError.Io(path, _) => path == parent
          case _                           => false)
    },
  )

  def spec = tests @@ TestAspect.withLiveSystem

  private def classpath: IO[Config.Error, List[Path]] =
    ZIO.config(Config.string("java.class.path")).map(_.split(java.io.File.pathSeparator).toList.map(Path.of(_)))

  private def temporary: ZIO[Scope, java.io.IOException, Path] =
    ZIO.acquireRelease(ZIO.attemptBlockingIO(Files.createTempDirectory("heddle-transcript-"))): path =>
      ZIO.attemptBlockingIO {
        val paths = Files.walk(path)
        try paths.iterator.asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
        finally paths.close()
      }.ignore

  private def request(repo: Path, stage: Path, cp: List[Path]): RunRequest =
    val id = PluginTaskId(TaskSpec.pluginId, TaskSpec.transcript.name)
    RunRequest(
      id,
      repo,
      Nil,
      PluginArgs.empty,
      Nil,
      InvocationContext(
        InvocationId.derive("transcript"),
        ClientId("transcript"),
        CommandIdentity.Task(id),
        histos.lock.ExecutionKind.Local,
        Interaction.Unattended,
        OfflineSelection.Offline,
      ),
      PluginEvents.closedInvocation,
      compiled = List(CellClasspath.mainOnly(ProjectName("docs"), ScalaVersion("3.10.1"), Jvm, cp)),
      destinations = OutputDestinations(List(OutputDestination.File(TranscriptLive.outputName, stage))),
    )
end TranscriptLiveSpec

private[heddle] object TranscriptFixture:
  val content: String =
    "package heddle.docs.ui\nobject HarnessTranscript:\n  val call = \"tools/call\"\n" +
      "object DatastarTranscript:\n  val signals = \"datastar-patch-signals\"\n" +
      "object SchemaTranscript:\n  val openApiWhere = \"components.schemas.Show\"\n"
