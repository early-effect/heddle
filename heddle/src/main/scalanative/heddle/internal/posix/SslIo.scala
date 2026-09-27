package heddle.internal.posix

import heddle.internal.openssl.{Ssl, SslError}
import zio.*

private[heddle] object SslIo:
  def handshake(session: Ssl.Session, accept: Boolean, fd: Int): IO[SslError | NetError, Unit] =
    def step: IO[SslError | NetError, Unit] =
      ZIO.suspendSucceed(ZIO.fromEither(session.handshake(accept))).flatMap {
        case None     => ZIO.unit
        case Some(on) => AsyncFd.ready(fd, on) *> step
      }
    step
end SslIo
