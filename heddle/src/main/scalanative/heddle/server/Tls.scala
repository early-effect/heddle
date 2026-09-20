package heddle.server

import heddle.error.HttpError
import heddle.internal.duplex.{ByteConn, NativeConn}
import heddle.internal.openssl.Ssl
import zio.*

final class Tls private (ctx: Ssl.Ctx):
  private[heddle] def listener(
      config: heddle.Server.Config
  ): ZIO[Scope, heddle.error.ServerError, heddle.internal.duplex.Listener] =
    heddle.internal.duplex.NativeListener.bind(config).map { plain =>
      new heddle.internal.duplex.Listener:
        def localPort = plain.localPort
        def accept    = plain.acceptFd.map(fd => NativeConn.of(fd))
        def close     = plain.close
    }

  private[heddle] def server(conn: ByteConn, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    val _ = alpn
    conn match
      case n: NativeConn =>
        ZIO
          .attemptBlockingInterrupt {
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
    ZLayer.fromZIO(
      ZIO.attempt(Tls(Ssl.serverCtx(certPem, keyPem))).mapError(HttpError.Io(_))
    )
