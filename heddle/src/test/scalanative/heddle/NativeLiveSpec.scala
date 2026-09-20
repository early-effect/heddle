package heddle

import heddle.server.Files
import java.nio.file.{Files as JFiles, Path}
import zio.*
import zio.test.*

object NativeLiveSpec extends ZIOSpecDefault:
  def spec =
    suite("Native live")(
      test("TLS handshake on a posix socket"):
        import heddle.internal.duplex.NativeListener
        import heddle.internal.openssl.Ssl
        import heddle.internal.posix.Net
        ZIO.scoped {
          for
            listener <- ZIO.acquireRelease(NativeListener.bind(local))(_.close)
            port     <- listener.localPort
            serverHs <- listener.acceptFd.flatMap { fd =>
              ZIO.attemptBlockingInterrupt {
                val ctx = Ssl.serverCtx(NativeTls.certPem, NativeTls.keyPem)
                Ssl.accept(ctx, fd)
              }
            }.fork
            clientHs <- ZIO.attemptBlockingInterrupt {
              val fd  = Net.connect("127.0.0.1", port)
              val ctx = Ssl.clientCtx()
              Ssl.connect(ctx, fd, "localhost")
            }
            serverS <- serverHs.join
            ping = Array[Byte]('p', 'i', 'n', 'g')
            n <- ZIO.attempt(clientHs.write(ping, 0, 4))
            buf = new Array[Byte](4)
            m <- ZIO.attempt(serverS.read(buf, 0, 4))
          yield
            clientHs.close()
            serverS.close()
            assertTrue(port > 0, n == 4, m == 4, buf.toSeq == ping.toSeq)
        }
      ,
      test("Server.install plus Client GET"):
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
      test("POST echo"):
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
                .flatMap { res =>
                  res.body.collect.map { bytes =>
                    assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ping")
                  }
                }
            }
          }
        }
      ,
      test("Files.fromPath serves a file"):
        val path = Path.of("target/heddle-native-files-test.txt")
        val _    = JFiles.writeString(path, "hello-native")
        Files.fromPath(path.toString).flatMap { res =>
          res.body.collect.map { bytes =>
            assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "hello-native")
          }
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(30.seconds) @@ TestAspect.withLiveClock

  private val local: Server.Config = Server.Config.default.copy(host = "127.0.0.1", port = 0)
end NativeLiveSpec
