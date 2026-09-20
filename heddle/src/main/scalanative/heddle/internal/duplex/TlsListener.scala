package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.Net
import zio.*

private[heddle] final class TlsListener(plain: NativeListener, ctx: Ssl.Ctx) extends Listener:
  def localPort: UIO[Int] = plain.localPort

  def accept: Task[ByteConn] =
    plain.acceptFd.flatMap { fd =>
      ZIO
        .attemptBlockingInterrupt(Ssl.accept(ctx, fd))
        .map(ssl => NativeConn.tls(fd, ssl))
        .tapError(_ => ZIO.succeed(Net.close(fd)))
    }

  def close: UIO[Unit] =
    plain.close *> ZIO.succeed(ctx.close())
end TlsListener

object TlsListener:
  def bind(config: Server.Config, certPem: String, keyPem: String): IO[ServerError, Listener] =
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
