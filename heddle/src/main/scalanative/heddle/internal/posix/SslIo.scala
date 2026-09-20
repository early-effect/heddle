package heddle.internal.posix

import heddle.internal.openssl.Ssl
import zio.*

private[heddle] object SslIo:
  def handshake(session: Ssl.Session, accept: Boolean, fd: Int): Task[Unit] =
    def step: Task[Unit] =
      ZIO.attempt(Ssl.handshake(session, accept)).flatMap {
        case 1  => ZIO.unit
        case -2 => AsyncFd.readable(fd) *> step
        case -3 => AsyncFd.writable(fd) *> step
        case n  => ZIO.fail(java.io.IOException(s"SSL handshake $n"))
      }
    step
end SslIo
