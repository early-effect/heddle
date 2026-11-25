package heddle.build

import histos.*
import histos.plugin.*
import zio.*

import java.io.IOException
import java.nio.file.{Files, Path}
import _root_.scala.util.Try

enum TranscriptError:
  case Destination(error: PluginRunError)
  case MissingMainClasspath
  case AmbiguousMainClasspath
  case EmptyMainClasspath
  case Settings(error: Config.Error)
  case Io(path: Path, cause: IOException)
  case Program(error: PluginRunError)
  case ProgramFailed(code: Int)
  case MissingFile(path: Path)

/** Runs the Main-only producer on the classpath supplied by the completed dependency. */
final class TranscriptLive extends DocsTranscriptTask:
  def run(task: TaskSpec, request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    if task != TaskSpec.transcript || request.id != PluginTaskId(TaskSpec.pluginId, TaskSpec.transcript.name) then
      ZIO.fail(PluginRunError.Unknown(request.id))
    else produce(request).mapError(TranscriptLive.atBoundary)

  private[build] def produce(request: RunRequest): ZIO[Scope, TranscriptError, PluginOutput] =
    for
      stage <- ZIO.fromEither(request.file(TranscriptLive.outputName)).mapError(TranscriptError.Destination(_))
      cell  <- request.compiled.filter(cell =>
        cell.module == ProjectName("docs") && cell.platform == Jvm && cell.scope == SourceSet.Main
      ) match
        case one :: Nil => ZIO.succeed(one)
        case Nil        => ZIO.fail(TranscriptError.MissingMainClasspath)
        case _          => ZIO.fail(TranscriptError.AmbiguousMainClasspath)
      _              <- ZIO.fail(TranscriptError.EmptyMainClasspath).when(cell.compile.isEmpty)
      javaExecutable <- ZIO.config(TranscriptLive.javaExecutable).mapError(TranscriptError.Settings(_))
      _              <- ZIO.foreachDiscard(Option(stage.toAbsolutePath.getParent)): parent =>
        ZIO.attemptBlockingIO(Files.createDirectories(parent)).mapError(TranscriptError.Io(parent, _))
      argv = List(
        javaExecutable.toString,
        "-cp",
        cell.compile.map(_.toAbsolutePath.normalize.toString).mkString(java.io.File.pathSeparator),
        "heddle.docs.fixture.WriteTranscript",
        stage.toAbsolutePath.normalize.toString,
      )
      code   <- Program.run(argv, request.repo, request.events).mapError(TranscriptError.Program(_))
      _      <- ZIO.fail(TranscriptError.ProgramFailed(code)).when(code != 0)
      exists <- ZIO.attemptBlockingIO(Files.isRegularFile(stage)).mapError(TranscriptError.Io(stage, _))
      _      <- ZIO.fail(TranscriptError.MissingFile(stage)).unless(exists)
    yield PluginOutput(
      "wrote the docs transcripts",
      List(WrittenFile(TranscriptLive.outputName, TranscriptLive.outputPath)),
    )
end TranscriptLive

object TranscriptLive:
  val outputName: OutputName            = OutputName("transcript-source")
  val outputPath: RepoPath              = RepoPath("target/generated/docs/HarnessTranscript.scala")
  val layer: ULayer[DocsTranscriptTask] = ZLayer.succeed(new TranscriptLive)

  private val javaExecutable: Config[Path] = Config
    .string("java.home")
    .mapOrFail: raw =>
      if raw.trim.isEmpty then Left(Config.Error.InvalidData(message = "java.home is empty"))
      else
        Try(Path.of(raw, "bin", "java")).toEither.left.map(_ => Config.Error.InvalidData(message = "invalid java.home"))

  private def atBoundary(error: TranscriptError): PluginRunError =
    error match
      case TranscriptError.Destination(error)     => error
      case TranscriptError.Program(error)         => error
      case TranscriptError.MissingMainClasspath   => refuse("docs JVM Main classpath is missing")
      case TranscriptError.AmbiguousMainClasspath => refuse("docs JVM Main classpath is ambiguous")
      case TranscriptError.EmptyMainClasspath     => refuse("docs JVM Main classpath is empty")
      case TranscriptError.Settings(error)        => refuse(s"transcript runner configuration failed: $error")
      case TranscriptError.Io(path, cause)     => refuse(s"transcript I/O failed at $path (${cause.getClass.getName})")
      case TranscriptError.ProgramFailed(code) => refuse(s"WriteTranscript exited $code")
      case TranscriptError.MissingFile(path)   => refuse(s"WriteTranscript did not write $path")

  private def refuse(message: String): PluginRunError =
    PluginDetail.from(message) match
      case Right(detail) => PluginRunError.Failed(detail)
      case Left(_)       => PluginRunError.Defect("PluginDetail")
end TranscriptLive
