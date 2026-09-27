package heddle

import heddle.error.HttpError
import heddle.internal.duplex.AcceptError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.Net
import heddle.server.Tls
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import zio.*
import zio.test.*

object NativeHttpsSpec extends ZIOSpecDefault:
  def spec =
    suite("Native HTTPS")(
      test("a plain accept then Tls.server answers a raw TLS GET"):
        ZIO.scoped {
          for
            listener <- NativeTls.listener(local)
            port     <- listener.localPort
            server   <- serveOne(listener).fork
            body     <- rawTlsGet(port, "/health")
            _        <- server.interrupt
          yield assertTrue(body.contains("200"), body.contains("ok"))
        }
      ,
      test("a plain accept then Tls.server answers a client that trusts the test certificate"):
        ZIO.scoped {
          for
            listener <- NativeTls.listener(local)
            port     <- listener.localPort
            server   <- serveOne(listener).fork
            res      <- Client.batched(Request.get(s"https://127.0.0.1:$port/health")).provideLayer(trusted)
            bytes    <- res.body.collect
            _        <- server.interrupt
          yield assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
        }
      ,
      test("Server.install plus a client that trusts the test certificate"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          ZIO
            .serviceWithZIO[Tls](Server.install(routes, local, _))
            .provideSomeLayer[Scope](Tls.pem(TestTls.certPem, TestTls.keyPem))
            .flatMap { server =>
              server.port.flatMap { port =>
                Client.batched(Request.get(s"https://127.0.0.1:$port/health")).provideLayer(trusted).flatMap { res =>
                  res.body.collect.map { bytes =>
                    assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
                  }
                }
              }
            }
        }
      ,
      test("the system trust store rejects a self-signed peer"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          ZIO
            .serviceWithZIO[Tls](Server.install(routes, local, _))
            .provideSomeLayer[Scope](Tls.pem(TestTls.certPem, TestTls.keyPem))
            .flatMap(_.port)
            .flatMap(port => Client.get(s"https://127.0.0.1:$port/health").either)
            .map(out =>
              assertTrue(out match
                case Left(_: ClientError.Tls) => true
                case _                        => false)
            )
        }
      ,
      test("a trusted certificate that names another host fails the handshake"):
        ZIO.scoped {
          for
            listener <- NativeTls.listener(local)
            port     <- listener.localPort
            server   <- listener.accept.fork
            fd       <- ZIO.fromEither(Net.connect("127.0.0.1", port))
            _        <- heddle.internal.posix.AsyncFd.writable(fd)
            session  <- ZIO.fromEither(
              Ssl.clientCtx(Some(TestTls.certPem)).flatMap(Ssl.connect(_, fd, "not-this-host.test"))
            )
            shake <- heddle.internal.posix.SslIo.handshake(session, accept = false, fd).either
            _     <- ZIO.succeed { session.close(); Net.close(fd) }
            _     <- server.interrupt
          yield assertTrue(shake.isLeft)
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val local: Server.Config =
    Server.Config.default.copy(host = "127.0.0.1", port = 0, http2 = false)

  private val trusted: Layer[ClientError, Client] =
    ZLayer.succeed(Client.Config.default) >>> heddle.client.ClientTls.trusting(TestTls.certPem)

  private def serveOne(listener: NativeTls.Listener): IO[AcceptError | HttpError | Throwable, Unit] =
    listener.accept.flatMap { conn =>
      val buf = ByteBuffer.allocate(1024)
      conn
        .read(buf)
        .flatMap { _ =>
          val res = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
          conn.write(Chunk.fromArray(res.getBytes(StandardCharsets.US_ASCII)))
        }
        .ensuring(conn.close)
    }

  private def rawTlsGet(port: Int, path: String): IO[Any, String] =
    ZIO.fromEither(Net.connect("127.0.0.1", port)).flatMap { fd =>
      heddle.internal.posix.AsyncFd.writable(fd) *>
        ZIO
          .fromEither(Ssl.clientCtx(Some(TestTls.certPem)).flatMap(Ssl.connect(_, fd, "localhost")))
          .flatMap { s =>
            heddle.internal.posix.SslIo.handshake(s, accept = false, fd) *> {
              val conn = heddle.internal.duplex.NativeConn.tls(fd, s)
              val req  = Chunk.fromArray(
                s"GET $path HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
                  .getBytes(StandardCharsets.US_ASCII)
              )
              val buf = java.nio.ByteBuffer.allocate(4096)
              (conn.write(req) *>
                conn.read(buf).map { n =>
                  buf.flip()
                  val arr = Array.ofDim[Byte](math.max(0, n))
                  buf.get(arr)
                  String(arr, StandardCharsets.US_ASCII)
                }).ensuring(conn.close)
            }
          }
    }
end NativeHttpsSpec
