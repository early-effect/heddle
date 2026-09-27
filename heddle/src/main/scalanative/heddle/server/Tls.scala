package heddle.server

import heddle.error.{HttpError, TlsError}
import heddle.internal.Pem
import heddle.internal.duplex.{ByteConn, NativeConn}
import heddle.internal.openssl.Ssl
import heddle.internal.posix.SslIo
import zio.*

final class Tls private (ctx: Ssl.Ctx):
  private[heddle] def server(conn: ByteConn, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    val _ = alpn
    conn match
      case n: NativeConn =>
        ZIO
          .attempt(Ssl.accept(ctx, n.fd))
          .flatMap { session =>
            SslIo.handshake(session, accept = true, n.fd).as(Tls.Session(NativeConn.tls(n.fd, session), ""))
          }
          .mapError(HttpError.Io(_))
      case _ =>
        ZIO.fail(HttpError.Io(IllegalArgumentException("TLS server needs a native connection")))
    end match
  end server
end Tls

object Tls:
  private[heddle] final class Session(val conn: ByteConn, val applicationProtocol: String)

  def pem(certPem: String, keyPem: String): ZLayer[Any, TlsError, Tls] =
    ZLayer.fromZIO(
      (ZIO.fromEither(Pem.body(certPem, "CERTIFICATE")) *> ZIO.fromEither(Pem.body(keyPem, "PRIVATE KEY"))) *>
        ZIO.attempt(Tls(Ssl.serverCtx(certPem, keyPem))).mapError(TlsError.Unusable(_))
    )
