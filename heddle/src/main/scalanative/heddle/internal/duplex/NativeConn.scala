package heddle.internal.duplex

import heddle.error.HttpError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.Net
import java.nio.ByteBuffer
import zio.*

private[heddle] final class NativeConn(val fd: Int, ssl: Option[Ssl.Session]) extends ByteConn:
  @volatile private var closed                  = false
  def read(dst: ByteBuffer): IO[HttpError, Int] =
    ZIO
      .attemptBlockingInterrupt {
        val n   = dst.remaining()
        val tmp = new Array[Byte](n)
        val got =
          ssl match
            case Some(s) => s.read(tmp, 0, n)
            case None    => Net.read(fd, tmp, 0, n)
        if got > 0 then dst.put(tmp, 0, got)
        if got == 0 then -1 else got
      }
      .mapError(HttpError.Io(_))

  def write(chunk: Chunk[Byte]): Task[Unit] =
    if chunk.isEmpty then ZIO.unit
    else
      ZIO.attemptBlockingInterrupt {
        val arr = chunk.toArray
        var off = 0
        while off < arr.length do
          val n =
            ssl match
              case Some(s) => s.write(arr, off, arr.length - off)
              case None    => Net.write(fd, arr, off, arr.length - off)
          if n <= 0 then throw java.io.IOException("write returned 0")
          off += n
      }

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
