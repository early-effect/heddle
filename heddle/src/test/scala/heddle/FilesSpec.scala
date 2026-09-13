package heddle

import java.nio.file.{Files as JFiles, Path}
import zio.*
import zio.test.*

object FilesSpec extends ZIOSpecDefault:
  def spec =
    suite("Files")(
      test("fromPath streams file bytes with Content-Length"):
        withTempFile("hello files", ".txt") { path =>
          Files.fromPath(path).flatMap { res =>
            res.body.collect.map { bytes =>
              assertTrue(
                res.status == Status.Ok,
                res.header("Content-Type").contains("text/plain; charset=utf-8"),
                res.body.length.contains(11L),
                bytes.toArray.toSeq == "hello files".getBytes.toSeq,
              )
            }
          }
        }
      ,
      test("Content-Type for html js css"):
        for
          html <- withTempFile("<p>x</p>", ".html")(p => Files.fromPath(p).map(_.header("Content-Type")))
          js   <- withTempFile("x", ".js")(p => Files.fromPath(p).map(_.header("Content-Type")))
          css  <- withTempFile("x", ".css")(p => Files.fromPath(p).map(_.header("Content-Type")))
        yield assertTrue(
          html.contains("text/html; charset=utf-8"),
          js.contains("text/javascript; charset=utf-8"),
          css.contains("text/css; charset=utf-8"),
        )
      ,
      test("missing path fails"):
        Files.fromPath(Path.of("/no/such/heddle-file-xyz")).either.map { e =>
          assertTrue(e.isLeft)
        }
      ,
      test("Handler.fromFile serves the file"):
        withTempFile("from handler", ".txt") { path =>
          val routes = Routes(Method.GET / "f" -> Handler.fromFile(path.toFile))
          routes.runZIO(Request.get("/f")).flatMap { res =>
            res.body.utf8.map { s =>
              assertTrue(res.status == Status.Ok, s == "from handler")
            }
          }
        }
      ,
      test("directory fails"):
        ZIO
          .attemptBlocking(JFiles.createTempDirectory("heddle-files"))
          .flatMap { dir =>
            Files.fromPath(dir).either.ensuring(ZIO.attemptBlocking(JFiles.deleteIfExists(dir)).orDie)
          }
          .map(e => assertTrue(e.isLeft)),
    ) @@ TestAspect.timeout(5.seconds)

  private def withTempFile[A](content: String, suffix: String)(use: Path => Task[A]): Task[A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking {
        val p = JFiles.createTempFile("heddle-files", suffix)
        JFiles.writeString(p, content)
        p
      }
    )(p => ZIO.attemptBlocking(JFiles.deleteIfExists(p)).orDie)(use)
end FilesSpec
