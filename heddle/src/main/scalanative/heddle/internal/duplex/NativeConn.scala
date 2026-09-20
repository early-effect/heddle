package heddle.internal.duplex

import heddle.error.HttpError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{AsyncFd, Net}
import java.nio.ByteBuffer
import zio.*

private[heddle] final class NativeConn(val fd: Int, ssl: Option[Ssl.Session]) extends ByteConn:
  @volatile private var closed                  = false
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
    AsyncFd.readable(fd).mapError(HttpError.Io(_))

  def close: UIO[Unit] =
    ZIO.succeed {
      if !closed then
        closed = true
        ssl match
          case Some(s) =>
            // SSL_set_fd transfers the fd to the BIO; SSL_free closes it.
            s.close()
          case None =>
            Net.close(fd)
    }

  def setReadTimeout(d: Duration): UIO[Unit] =
    ZIO.succeed {
      ssl match
        case Some(_) =>
          // SSL_set_fd owns the socket. SO_RCVTIMEO after that stalls SSL_read on Native.
          ()
        case None =>
          val ms =
            if d == Duration.Infinity || d.toNanos <= 0L then 0
            else math.max(1L, d.toMillis).min(Int.MaxValue.toLong).toInt
          Net.setRecvTimeout(fd, ms)
    }
end NativeConn

object NativeConn:
  def of(fd: Int): NativeConn = NativeConn(fd, None)

  def tls(fd: Int, session: Ssl.Session): NativeConn = NativeConn(fd, Some(session))
