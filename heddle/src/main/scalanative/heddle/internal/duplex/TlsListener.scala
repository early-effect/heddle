package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{Net, SslIo}
import zio.*

private[heddle] final class TlsListener(plain: NativeListener, ctx: Ssl.Ctx) extends Listener:
  def localPort: UIO[Int] = plain.localPort

  def accept: Task[ByteConn] =
    plain.acceptFd.flatMap { fd =>
      ZIO
        .attempt(Ssl.accept(ctx, fd))
        .flatMap { session =>
          SslIo.handshake(session, accept = true, fd).as(NativeConn.tls(fd, session))
        }
        .tapError(_ => ZIO.succeed(Net.close(fd)))
    }

  def close: UIO[Unit] =
    // Leave ctx alive until process exit. SSL_CTX_free while any SSL* from
    // this ctx is still live is an OpenSSL abort.
    plain.close
end TlsListener

object TlsListener:
  def bind(config: Server.Config, certPem: String, keyPem: String): ZIO[Scope, ServerError, Listener] =
    NativeListener.bind(config).flatMap { plain =>
      ZIO
        .attempt(Ssl.serverCtx(certPem, keyPem))
        .mapBoth(
          e => ServerError.BindFailed(config.host, config.port, e),
          ctx => TlsListener(plain, ctx),
        )
        .tapError(_ => plain.close)
    }
end TlsListener
