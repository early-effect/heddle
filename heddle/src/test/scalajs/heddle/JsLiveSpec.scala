package heddle

import heddle.internal.node.{Buffers, Fs, NodeTls, TlsOptions, TlsSocket}
import heddle.server.{Files, Tls}
import scala.scalajs.js
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
                assertTrue(res.status == Status.Ok, res.body.asString == "ok")
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
                .map(res => assertTrue(res.body.asString == "ping"))
            }
          }
        }
      ,
      test("Files.fromPath serves a Node file"):
        val path = "target/heddle-js-files-test.txt"
        val _    = Fs.writeFileSync(path, Buffers.toU8(Chunk.fromArray("hello-js".getBytes)))
        Files.fromPath(path).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString == "hello-js")
        }
      ,
      test("HTTPS Server.install answers HTTP/1.1"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          Server
            .install(routes, local)
            .provideSomeLayer[Scope](Tls.pem(JsTls.certPem, JsTls.keyPem))
            .flatMap { server =>
              server.port.flatMap { port =>
                tlsGet(port, "/health").map { body =>
                  assertTrue(body.contains("200"), body.contains("ok"))
                }
              }
            }
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val local: Server.Config = Server.Config.default.copy(host = "127.0.0.1", port = 0)

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
        end drain
        (conn.write(req) *> drain).ensuring(conn.close)
      }
end JsLiveSpec
