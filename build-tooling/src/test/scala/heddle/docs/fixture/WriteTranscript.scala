package heddle.docs.fixture

import heddle.build.TranscriptFixture
import zio.*

import java.io.IOException
import java.nio.file.{Files, Path}

/** Invocation fixture, not the docs producer. */
object WriteTranscript extends ZIOAppDefault:
  enum Fail:
    case Arguments
    case InvalidPath(cause: Throwable)
    case Io(cause: IOException)

  def run =
    for
      args <- ZIO.serviceWith[ZIOAppArgs](_.getArgs)
      raw  <- ZIO
        .fromOption(args.toList match
          case raw :: Nil => Some(raw)
          case _          => None)
        .orElseFail(Fail.Arguments)
      path <- ZIO.attempt(Path.of(raw)).mapError(Fail.InvalidPath(_))
      omit <- ZIO.attemptBlockingIO(Files.exists(Path.of("omit-output"))).mapError(Fail.Io(_))
      _    <- ZIO.unless(omit)(
        ZIO.attemptBlockingIO(Files.writeString(path, TranscriptFixture.content)).mapError(Fail.Io(_)).unit
      )
    yield ()
end WriteTranscript
