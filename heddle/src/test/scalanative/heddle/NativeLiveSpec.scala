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
            listener <- NativeListener.bind(local)
            port     <- listener.localPort
            serverHs <- listener.acceptFd.flatMap { fd =>
              ZIO
                .attempt {
                  val ctx = Ssl.serverCtx(NativeTls.certPem, NativeTls.keyPem)
                  Ssl.accept(ctx, fd)
                }
                .flatMap(s => heddle.internal.posix.SslIo.handshake(s, accept = true, fd).as((fd, s)))
            }.fork
            clientHs <- ZIO.attempt(Net.connect("127.0.0.1", port)).flatMap { fd =>
              heddle.internal.posix.AsyncFd.writable(fd) *>
                ZIO
                  .attempt {
                    val ctx = Ssl.clientCtx()
                    Ssl.connect(ctx, fd, "localhost")
                  }
                  .flatMap(s => heddle.internal.posix.SslIo.handshake(s, accept = false, fd).as((fd, s)))
            }
            (serverFd, serverS) <- serverHs.join
            (clientFd, clientS) = clientHs
            ping                = Chunk.fromArray(Array[Byte]('p', 'i', 'n', 'g'))
            client              = heddle.internal.duplex.NativeConn.tls(clientFd, clientS)
            server              = heddle.internal.duplex.NativeConn.tls(serverFd, serverS)
            _ <- client.write(ping)
            buf = java.nio.ByteBuffer.allocate(4)
            m <- server.read(buf).mapError(e => java.io.IOException(e.message))
          yield
            clientS.close()
            serverS.close()
            buf.flip()
            val got = Array.ofDim[Byte](m)
            buf.get(got)
            assertTrue(port > 0, m == 4, got.toSeq == ping.toArray.toSeq)
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
