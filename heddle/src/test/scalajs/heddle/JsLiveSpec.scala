package heddle

import heddle.internal.node.{Buffers, Fs, NodePath, NodeTls, StatOptions, TlsOptions, TlsSocket}
import heddle.server.{BodyBytes, Files, Port, Tls}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import zio.*
import zio.test.*

object JsLiveSpec extends ZIOSpecDefault:
  def spec =
    suite("JS live")(
      test("Server.install plus fetch Client GET"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          Server.install(routes, local).flatMap { server =>
            server.port.flatMap { port =>
              Client.get(s"http://127.0.0.1:$port/health").map { res =>
                assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "ok")
              }
            }
          }
        }
      ,
      test("POST echo over fetch"):
        val routes = Routes(
          Method.POST / "echo" -> Handler.fromFunctionZIO { req =>
            req.body.utf8.orDie.map(Response.text(_))
          }
        )
        ZIO.scoped {
          Server.install(routes, local).flatMap { server =>
            server.port.flatMap { port =>
              Client
                .request(Method.POST, s"http://127.0.0.1:$port/echo", body = Body.text("ping"))
                .map(res => assertTrue(res.body.text.is(_.some) == "ping"))
            }
          }
        }
      ,
      test("a body past maxBodyBytes is BodyTooLarge over fetch"):
        val routes = Routes(Method.GET / "big" -> Handler.text("x" * 4096))
        ZIO.scoped {
          Server.install(routes, local).flatMap { server =>
            server.port.flatMap { port =>
              Client
                .batched(Request.get(s"http://127.0.0.1:$port/big"))
                .provideLayer(
                  ZLayer.succeed(Client.Config.default.copy(maxBodyBytes = BodyBytes(1024L))) >>> Client.layer
                )
                .either
                .map(out =>
                  assertTrue(out match
                    case Left(ClientError.Protocol(_, HttpError.BodyTooLarge)) => true
                    case _                                                     => false)
                )
            }
          }
        }
      ,
      test("a refused connection is Connect over fetch"):
        val free = ZIO.scoped(Server.install(Routes.empty, local).flatMap(_.port))
        free.flatMap { port =>
          Client
            .get(s"http://127.0.0.1:$port/")
            .either
            .map(out =>
              assertTrue(out match
                case Left(_: ClientError.Connect) => true
                case _                            => false)
            )
        }
      ,
      test("Files.fromPath serves a Node file"):
        ZIO.scoped:
          for
            file <- temporaryFile
            res  <- Files.fromPath(file.path)
          yield assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "hello-js")
      ,
      test("interrupting the Node file fixture removes its directory"):
        ZIO.scoped:
          for
            ready <- Promise.make[NodeFixtureError, NodeFile]
            fiber <- ZIO
              .scoped(temporaryFile.tap(file => ready.succeed(file)).flatMap(_ => ZIO.never))
              .tapErrorCause(cause => ready.failCause(cause).unit)
              .forkScoped
            file   <- ready.await
            exit   <- fiber.interrupt
            exists <- ZIO
              .attempt(Fs.statSync(file.directory, StatOptions(throwIfNoEntry = false)).toOption.nonEmpty)
              .mapError(NodeFixtureError.Inspect(file.directory, _))
          yield assertTrue(exit.isInterrupted, !exists)
      ,
      test("HTTPS Server.install answers HTTP/1.1"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          ZIO
            .serviceWithZIO[Tls](Server.install(routes, local, _))
            .provideSomeLayer[Scope](Tls.pem(TestTls.certPem, TestTls.keyPem))
            .flatMap { server =>
              server.port.flatMap { port =>
                tlsGet(port, "/health").map { body =>
                  assertTrue(body.contains("200"), body.contains("ok"))
                }
              }
            }
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val local: Server.Config = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))

  private final case class NodeFile(directory: String, path: String)

  private enum NodeFixtureError:
    case Create(cause: Throwable)
    case Write(path: String, cause: Throwable)
    case Remove(path: String, cause: Throwable)
    case Inspect(path: String, cause: Throwable)

  private def temporaryFile: ZIO[Scope, NodeFixtureError, NodeFile] =
    ZIO
      .acquireRelease(
        ZIO
          .attempt(NodeFs.mkdtempSync(NodePath.join(NodeOs.tmpdir(), "heddle-js-files-")))
          .mapError(NodeFixtureError.Create(_))
      )(directory =>
        ZIO
          .attempt(NodeFs.rmSync(directory, RmRecursive()))
          .mapError(NodeFixtureError.Remove(directory, _))
          .catchAll(error => ZIO.logWarning(s"Node file fixture cleanup failed: $error"))
      )
      .flatMap: directory =>
        val path = NodePath.join(directory, "fixture.txt")
        ZIO
          .attempt(Fs.writeFileSync(path, Buffers.toU8(Chunk.fromArray("hello-js".getBytes))))
          .mapError(NodeFixtureError.Write(path, _))
          .as(NodeFile(directory, path))

  @js.native
  @JSImport("node:fs", JSImport.Namespace)
  private object NodeFs extends js.Object:
    def mkdtempSync(prefix: String): String            = js.native
    def rmSync(path: String, options: js.Object): Unit = js.native

  @js.native
  @JSImport("node:os", JSImport.Namespace)
  private object NodeOs extends js.Object:
    def tmpdir(): String = js.native

  private class RmRecursive extends js.Object:
    val recursive: Boolean = true
    val force: Boolean     = true

  private def tlsGet(port: Int, path: String): Task[String] =
    ZIO
      .async[Any, Throwable, TlsSocket] { cb =>
        val opts = TlsOptions(
          host = "127.0.0.1",
          port = port,
          rejectUnauthorized = false,
          servername = "localhost",
        )
        var sock: TlsSocket = null
        sock = NodeTls.connect(opts, () => cb(ZIO.succeed(sock)))
        sock.once(
          "error",
          (
              (err: js.Error) => cb(ZIO.fail(java.io.IOException(Option(err.message).getOrElse("tls connect"))))
          ): js.Function1[js.Error, Unit],
        )
        ()
      }
      .flatMap { sock =>
        val conn = heddle.internal.duplex.NodeConn.tls(sock)
        val req  = Chunk.fromArray(s"GET $path HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".getBytes)
        val buf  = java.nio.ByteBuffer.allocate(4096)
        val acc  = new java.io.ByteArrayOutputStream()
        def drain: Task[String] =
          buf.clear()
          conn.read(buf).mapError(e => java.io.IOException(e.message)).flatMap { n =>
            if n < 0 then ZIO.succeed(acc.toString("UTF-8"))
            else
              buf.flip()
              val arr = Array.ofDim[Byte](n)
              buf.get(arr)
              acc.write(arr)
              drain
          }
        (conn.write(req).mapError(_.asThrowable) *> drain).ensuring(conn.close)
      }
end JsLiveSpec
