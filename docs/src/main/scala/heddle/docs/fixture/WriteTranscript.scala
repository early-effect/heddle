package heddle.docs.fixture

import heddle.docs.ui.{HarnessScript, SchemaPicture}
import zio.*

import java.io.IOException
import java.nio.file.{Files, Path}

/** Writes the strings the shared docs page renders. */
object WriteTranscript extends ZIOAppDefault:

  enum Fail:
    case Harness(gap: HarnessScript.Gap)
    case Frames(gap: DatastarFrames.Gap)
    case Schema(gap: SchemaPicture.Gap)
    case InvalidPath(cause: Throwable)
    case Io(path: Path, cause: IOException)

    def describe: String =
      this match
        case Harness(gap)       => gap.toString
        case Frames(gap)        => gap.toString
        case Schema(gap)        => gap.toString
        case InvalidPath(cause) => s"invalid transcript path (${cause.getClass.getName})"
        case Io(path, cause)    => s"could not write the transcript at $path (${cause.getClass.getName})"
  end Fail

  def run =
    ZIO.serviceWithZIO[ZIOAppArgs] { args =>
      args.getArgs.toList match
        case path :: Nil =>
          ZIO
            .attempt(Path.of(path))
            .mapError(Fail.InvalidPath(_))
            .flatMap(write)
            .foldZIO(
              fail => Console.printLineError(fail.describe).ignore.as(ExitCode.failure),
              _ => ZIO.succeed(ExitCode.success),
            )
        case _ =>
          Console.printLineError("usage: WriteTranscript <HarnessTranscript.scala>").ignore.as(ExitCode.failure)
    }

  def write(path: Path): IO[Fail, Unit] =
    for
      script <- HarnessScript.load.mapError(Fail.Harness(_))
      frames <- DatastarFrames.load.mapError(Fail.Frames(_))
      schema <- SchemaPicture.load.mapError(Fail.Schema(_))
      text = HarnessScript.source(script) + "\n" + DatastarFrames.source(frames) + "\n" + SchemaPicture.source(schema)
      _ <- ZIO.attemptBlockingIO(Files.writeString(path, text)).mapError(Fail.Io(path, _))
    yield ()
end WriteTranscript
