package heddle.server

import heddle.error.{HttpError, TlsError}
import heddle.internal.Pem
import heddle.internal.duplex.{ByteConn, NativeConn}
import heddle.internal.openssl.Ssl
import heddle.internal.posix.SslIo
import zio.*

final class Tls private (ctx: Ssl.Ctx):
  /** Native serves HTTP/1.1 only, so there is no ALPN to offer. */
  private[heddle] def server(conn: NativeConn): IO[HttpError, Tls.Session] =
    ZIO
      .suspendSucceed(ZIO.fromEither(Ssl.accept(ctx, conn.fd)))
      .flatMap { session =>
        SslIo
          .handshake(session, accept = true, conn.fd)
          .onError(_ => ZIO.succeed(session.close()))
          .as(Tls.Session(NativeConn.tls(conn.fd, session), ""))
      }
      .mapError(e => HttpError.Io(e.exception))
end Tls

object Tls:
  private[heddle] final class Session(val conn: ByteConn, val applicationProtocol: String)

  def pem(certPem: String, keyPem: String): ZLayer[Any, TlsError, Tls] =
    ZLayer.fromZIO(
      (ZIO.fromEither(Pem.body(certPem, "CERTIFICATE")) *> ZIO.fromEither(Pem.body(keyPem, "PRIVATE KEY"))) *>
        ZIO
          .suspendSucceed(ZIO.fromEither(Ssl.serverCtx(certPem, keyPem)))
          .mapBoth(e => TlsError.Unusable(e.exception), Tls(_))
    )
end Tls
