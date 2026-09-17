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
          .map(e => assertTrue(e.isLeft))
      ,
      test("If-None-Match matching etag is 304"):
        withTempFile("cache-me", ".txt") { path =>
          Files.fromPath(path).flatMap { first =>
            val tag = first.header("ETag").get
            Files.fromPath(path, Request.get("/x").withHeader("If-None-Match", tag)).map { res =>
              assertTrue(res.status == Status.NotModified, res.body.isEmpty)
            }
          }
        }
      ,
      test("Range bytes=0-4 is 206"):
        withTempFile("0123456789", ".txt") { path =>
          Files.fromPath(path, Request.get("/x").withHeader("Range", "bytes=0-4")).flatMap { res =>
            res.body.utf8.map { s =>
              assertTrue(
                res.status == Status.PartialContent,
                s == "01234",
                res.header("Content-Range").contains("bytes 0-4/10"),
              )
            }
          }
        }
      ,
      test("Range bytes=6-8 seeks instead of reading from the start"):
        withTempFile("0123456789", ".txt") { path =>
          Files.fromPath(path, Request.get("/x").withHeader("Range", "bytes=6-8")).flatMap { res =>
            res.body.utf8.map { s =>
              assertTrue(
                res.status == Status.PartialContent,
                s == "678",
                res.header("Content-Range").contains("bytes 6-8/10"),
              )
            }
          }
        }
      ,
      test("directory jail rejects .."):
        withTempDir { dir =>
          val secret = dir.resolve("secret.txt")
          val pub    = dir.resolve("pub")
          ZIO.attemptBlocking {
            JFiles.createDirectories(pub)
            JFiles.writeString(secret, "nope")
            JFiles.writeString(pub.resolve("ok.txt"), "ok")
          } *> {
            val routes = Routes.fromHandler(Handler.text("fallback")) @@ Middleware.serveDirectory("/static", pub)
            for
              escaped <- routes(Request.get("/static/../secret.txt"))
              direct  <- Files.fromDirectory(pub, "/static", Request.get("/static/../secret.txt"))
            yield assertTrue(
              escaped.body.asString == "fallback",
              direct.isEmpty,
            )
          }
        }
      ,
      test("serveDirectory serves a file"):
        withTempDir { dir =>
          ZIO.attemptBlocking(JFiles.writeString(dir.resolve("a.txt"), "hello dir")) *> {
            val routes = Routes.empty @@ Middleware.serveDirectory("/static", dir)
            routes(Request.get("/static/a.txt")).flatMap { res =>
              res.body.utf8.map(s => assertTrue(res.status == Status.Ok, s == "hello dir"))
            }
          }
        }
      ,
      test("requestLog still returns the handler response"):
        val routes = Routes(Method.GET / "x" -> Handler.text("ok")) @@ Middleware.requestLog
        routes(Request.get("/x")).map(res => assertTrue(res.body.asString == "ok")),
    ) @@ TestAspect.timeout(5.seconds)

  private def withTempFile[A](content: String, suffix: String)(use: Path => Task[A]): Task[A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking {
        val p = JFiles.createTempFile("heddle-files", suffix)
        JFiles.writeString(p, content)
        p
      }
    )(p => ZIO.attemptBlocking(JFiles.deleteIfExists(p)).orDie)(use)

  private def withTempDir[A](use: Path => Task[A]): Task[A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(JFiles.createTempDirectory("heddle-dir"))
    )(dir =>
      ZIO.attemptBlocking {
        JFiles.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(JFiles.deleteIfExists(_))
      }.orDie
    )(use)
end FilesSpec
