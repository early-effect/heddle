package heddle.internal.duplex

import heddle.error.HttpError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{AsyncFd, Net}
import java.nio.ByteBuffer
import zio.*

/** A non-blocking socket, plain or TLS. `SO_RCVTIMEO` never fires on a non-blocking socket, so the read timeout bounds
  * each park on the poller instead.
  */
private[heddle] final class NativeConn(val fd: Int, ssl: Option[Ssl.Session]) extends ByteConn:
  @volatile private var closed                  = false
  @volatile private var readTimeout: Duration   = Duration.Infinity
  def read(dst: ByteBuffer): IO[HttpError, Int] =
    def attempt: IO[HttpError, Option[Int]] =
      ZIO
        .attempt {
          val n   = dst.remaining()
          val tmp = new Array[Byte](n)
          val got =
            ssl match
              case Some(s) => s.read(tmp, 0, n)
              case None    => Net.read(fd, tmp, 0, n)
          if got == -2 || got == -3 then None
          else
            if got > 0 then dst.put(tmp, 0, got)
            Some(if got == 0 then -1 else got)
        }
        .mapError(HttpError.Io(_))
    def loop: IO[HttpError, Int] =
      attempt.flatMap {
        case Some(n) => ZIO.succeed(n)
        case None    => parkRead *> loop
      }
    loop
  end read

  def write(chunk: Chunk[Byte]): Task[Unit] =
    if chunk.isEmpty then ZIO.unit
    else
      val arr                        = chunk.toArray
      def loop(off: Int): Task[Unit] =
        if off >= arr.length then ZIO.unit
        else
          ZIO
            .attempt {
              ssl match
                case Some(s) => s.write(arr, off, arr.length - off)
                case None    => Net.write(fd, arr, off, arr.length - off)
            }
            .flatMap { n =>
              if n == -2 then AsyncFd.readable(fd) *> loop(off)
              else if n == -3 then AsyncFd.writable(fd) *> loop(off)
              else if n <= 0 then ZIO.fail(java.io.IOException("write returned 0"))
              else loop(off + n)
            }
      loop(0)

  private def parkRead: IO[HttpError, Unit] =
    val park    = AsyncFd.readable(fd).mapError(HttpError.Io(_))
    val timeout = readTimeout
    if timeout == Duration.Infinity || timeout.toNanos <= 0L then park
    else park.timeoutFail(HttpError.Timeout)(timeout)

  def close: UIO[Unit] =
    ZIO.succeed {
      if !closed then
        closed = true
        // SSL_set_fd wraps the socket in a BIO_NOCLOSE BIO: SSL_free leaves it open.
        ssl.foreach(_.close())
        Net.close(fd)
    }

  def setReadTimeout(d: Duration): UIO[Unit] =
    ZIO.succeed { readTimeout = d }
end NativeConn

object NativeConn:
  def of(fd: Int): NativeConn = NativeConn(fd, None)

  def tls(fd: Int, session: Ssl.Session): NativeConn = NativeConn(fd, Some(session))
