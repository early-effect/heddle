package heddle.docs.fixture

import heddle.docs.ui.{HarnessScript, SchemaPicture}
import zio.*

import java.nio.file.{Files, Path}

/** Writes the strings the shared docs page renders. */
object WriteTranscript extends ZIOAppDefault:

  private enum Fail:
    case Harness(gap: HarnessScript.Gap)
    case Frames(gap: DatastarFrames.Gap)
    case Schema(gap: SchemaPicture.Gap)
    case Io(kind: String)

    def describe: String =
      this match
        case Harness(gap) => gap.toString
        case Frames(gap)  => gap.toString
        case Schema(gap)  => gap.toString
        case Io(kind)     => s"could not write the transcript ($kind)"
  end Fail

  def run =
    ZIO.serviceWithZIO[ZIOAppArgs] { args =>
      args.getArgs.toList match
        case path :: Nil =>
          write(Path.of(path)).foldZIO(
            fail => Console.printLineError(fail.describe).as(ExitCode.failure),
            _ => ZIO.succeed(ExitCode.success),
          )
        case _ =>
          Console.printLineError("usage: WriteTranscript <HarnessTranscript.scala>").as(ExitCode.failure)
    }

  private def write(path: Path): IO[Fail, Unit] =
    for
      script <- HarnessScript.load.mapError(Fail.Harness(_))
      frames <- DatastarFrames.load.mapError(Fail.Frames(_))
      schema <- SchemaPicture.load.mapError(Fail.Schema(_))
      text = HarnessScript.source(script) + "\n" + DatastarFrames.source(frames) + "\n" + SchemaPicture.source(schema)
      _ <- ZIO.attempt(Files.writeString(path, text)).mapError(t => Fail.Io(t.getClass.getName))
    yield ()
end WriteTranscript
