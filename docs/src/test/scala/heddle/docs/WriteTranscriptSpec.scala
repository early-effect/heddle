package heddle.docs

import heddle.docs.fixture.{DatastarFrames, WriteTranscript}
import heddle.docs.ui.{HarnessScript, SchemaPicture}
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

object WriteTranscriptSpec extends ZIOSpecDefault:
  def spec = suite("transcript producer")(
    test("one file preserves every field of all three generated objects") {
      ZIO.scoped:
        for
          file     <- temporary
          harness  <- HarnessScript.load
          datastar <- DatastarFrames.load
          schema   <- SchemaPicture.load
          _        <- WriteTranscript.write(file)
          text     <- ZIO.attemptBlockingIO(Files.readString(file))
        yield assertTrue(
          text == HarnessScript.source(harness) + "\n" + DatastarFrames.source(datastar) + "\n" + SchemaPicture.source(
            schema
          ),
          text.contains("object HarnessTranscript:"),
          text.contains("object DatastarTranscript:"),
          text.contains("object SchemaTranscript:"),
        )
    },
    test("an unwritable destination retains the path and I/O cause") {
      ZIO.scoped:
        for
          file  <- temporary
          error <- WriteTranscript.write(file.resolve("child")).flip
        yield assertTrue(error match
          case WriteTranscript.Fail.Io(path, _) => path == file.resolve("child")
          case _                                => false)
    },
  )

  private def temporary: ZIO[Scope, java.io.IOException, Path] =
    ZIO.acquireRelease(ZIO.attemptBlockingIO(Files.createTempFile("heddle-transcripts-", ".scala")))(path =>
      ZIO.attemptBlockingIO(Files.deleteIfExists(path)).ignore
    )
end WriteTranscriptSpec
