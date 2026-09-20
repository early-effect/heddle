package heddle

import heddle.internal.duplex.TlsListener
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
      test("getDynamic sees Tls from pem layer"):
        ZIO.scoped {
          ZIO
            .environmentWith[Any](_.getDynamic[Tls])
            .provideSomeLayer[Scope](Tls.pem(NativeTls.certPem, NativeTls.keyPem))
            .map(got => assertTrue(got.isDefined))
        }
      ,
      test("TlsListener plus raw TLS GET"):
        ZIO.scoped {
          for
            listener <- TlsListener.bind(local, NativeTls.certPem, NativeTls.keyPem)
            port     <- listener.localPort
            server   <- serveOne(listener).fork
            body     <- rawTlsGet(port, "/health")
            _        <- server.interrupt
          yield assertTrue(body.contains("200"), body.contains("ok"))
        }
      ,
      test("TlsListener plus Client GET over TLS"):
        ZIO.scoped {
          for
            listener <- TlsListener.bind(local, NativeTls.certPem, NativeTls.keyPem)
            port     <- listener.localPort
            server   <- serveOne(listener).fork
            res      <- Client.get(s"https://127.0.0.1:$port/health")
            bytes    <- res.body.collect
            _        <- server.interrupt
          yield assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
        }
      ,
      test("Server.install plus Client GET over TLS"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        ZIO.scoped {
          Server
            .install(routes, local)
            .provideSomeLayer[Scope](Tls.pem(NativeTls.certPem, NativeTls.keyPem))
            .flatMap { server =>
              server.port.flatMap { port =>
                Client.get(s"https://127.0.0.1:$port/health").flatMap { res =>
                  res.body.collect.map { bytes =>
                    assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
                  }
                }
              }
            }
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val local: Server.Config =
    Server.Config.default.copy(host = "127.0.0.1", port = 0, http2 = false)

  private def serveOne(listener: heddle.internal.duplex.Listener): Task[Unit] =
    listener.accept.flatMap { conn =>
      val buf = ByteBuffer.allocate(1024)
      conn
        .read(buf)
        .mapError(e => java.io.IOException(e.message))
        .flatMap { _ =>
          val res = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
          conn.write(Chunk.fromArray(res.getBytes(StandardCharsets.US_ASCII)))
        }
        .ensuring(conn.close)
    }

  private def rawTlsGet(port: Int, path: String): Task[String] =
    ZIO.attempt(Net.connect("127.0.0.1", port)).flatMap { fd =>
      heddle.internal.posix.AsyncFd.writable(fd) *>
        ZIO
          .attempt {
            val ctx = Ssl.clientCtx()
            Ssl.connect(ctx, fd, "localhost")
          }
          .flatMap { s =>
            heddle.internal.posix.SslIo.handshake(s, accept = false, fd) *> {
              val conn = heddle.internal.duplex.NativeConn.tls(fd, s)
              val req  = Chunk.fromArray(
                s"GET $path HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
                  .getBytes(StandardCharsets.US_ASCII)
              )
              val buf = java.nio.ByteBuffer.allocate(4096)
              (conn.write(req) *>
                conn.read(buf).mapError(e => java.io.IOException(e.message)).map { n =>
                  buf.flip()
                  val arr = Array.ofDim[Byte](math.max(0, n))
                  buf.get(arr)
                  String(arr, StandardCharsets.US_ASCII)
                }).ensuring(conn.close)
            }
          }
    }
end NativeHttpsSpec
