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
            listener <- ZIO.acquireRelease(
              TlsListener.bind(local, NativeTls.certPem, NativeTls.keyPem)
            )(_.close)
            port   <- listener.localPort
            server <- serveOne(listener).fork
            body   <- rawTlsGet(port, "/health")
            _      <- server.interrupt
          yield assertTrue(body.contains("200"), body.contains("ok"))
        }
      ,
      test("TlsListener plus Client GET over TLS"):
        ZIO.scoped {
          for
            listener <- ZIO.acquireRelease(
              TlsListener.bind(local, NativeTls.certPem, NativeTls.keyPem)
            )(_.close)
            port   <- listener.localPort
            server <- serveOne(listener).fork
            res    <- Client.get(s"https://127.0.0.1:$port/health")
            bytes  <- res.body.collect
            _      <- server.interrupt
          yield assertTrue(res.status == Status.Ok, String(bytes.toArray, "UTF-8") == "ok")
        },
    ) @@ TestAspect.sequential @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock

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
    ZIO.attemptBlockingInterrupt {
      val fd  = Net.connect("127.0.0.1", port)
      val ctx = Ssl.clientCtx()
      val s   = Ssl.connect(ctx, fd, "localhost")
      try
        val req = s"GET $path HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          .getBytes(StandardCharsets.US_ASCII)
        val _   = s.write(req, 0, req.length)
        val buf = new Array[Byte](4096)
        val n   = s.read(buf, 0, buf.length)
        String(buf, 0, math.max(0, n), StandardCharsets.US_ASCII)
      finally s.close()
    }
end NativeHttpsSpec
