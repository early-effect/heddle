package heddle.server

import heddle.error.HttpError
import heddle.internal.duplex.{ByteConn, NativeConn}
import heddle.internal.openssl.Ssl
import zio.*

final class Tls private (private[heddle] val certPem: String, private[heddle] val keyPem: String):
  private[heddle] def listener(
      config: heddle.Server.Config
  ): IO[heddle.error.ServerError, heddle.internal.duplex.Listener] =
    heddle.internal.duplex.TlsListener.bind(config, certPem, keyPem)

  private[heddle] def server(conn: ByteConn, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    val _ = alpn
    conn match
      case n: NativeConn =>
        ZIO
          .attemptBlockingInterrupt {
            val ctx     = Ssl.serverCtx(certPem, keyPem)
            val session = Ssl.accept(ctx, n.fd)
            Tls.Session(NativeConn.tls(n.fd, session), "")
          }
          .mapError(HttpError.Io(_))
      case _ =>
        ZIO.fail(HttpError.Io(IllegalArgumentException("TLS server needs a native connection")))
    end match
  end server
end Tls

object Tls:
  private[heddle] final class Session(val conn: ByteConn, val applicationProtocol: String)

  def pem(certPem: String, keyPem: String): ZLayer[Any, HttpError, Tls] =
    ZLayer.succeed(Tls(certPem, keyPem))
