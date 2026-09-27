package heddle.internal.duplex

import heddle.error.HttpError
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{AsyncFd, FfiError, Interest, Net, Transfer}
import java.nio.ByteBuffer
import zio.*

/** A non-blocking socket, plain or TLS. `SO_RCVTIMEO` never fires on a non-blocking socket, so the read timeout bounds
  * each park on the poller instead.
  */
private[heddle] final class NativeConn(val fd: Int, ssl: Option[Ssl.Session]) extends ByteConn:
  @volatile private var closed                = false
  @volatile private var readTimeout: Duration = Duration.Infinity

  /** The bytes read, or `-1` at end of stream. */
  def read(dst: ByteBuffer): IO[HttpError, Int] =
    ZIO.suspendSucceed {
      val n                                = dst.remaining()
      val tmp                              = new Array[Byte](n)
      def pull: Either[FfiError, Transfer] =
        ssl match
          case Some(s) => s.read(tmp, 0, n)
          case None    => Net.read(fd, tmp, 0, n)
      def loop: IO[HttpError, Int] =
        ZIO.suspendSucceed(ZIO.fromEither(pull)).mapError(e => HttpError.Io(e.exception)).flatMap {
          case Transfer.Moved(got)  => ZIO.succeed { dst.put(tmp, 0, got); got }
          case Transfer.Eof         => ZIO.succeed(-1)
          case Transfer.Blocked(on) => park(on) *> loop
        }
      loop
    }

  /** Plain sockets block a write on writability; TLS may block it on either direction. */
  def write(chunk: Chunk[Byte]): Task[Unit] =
    if chunk.isEmpty then ZIO.unit
    else
      val arr                                        = chunk.toArray
      def push(off: Int): Either[FfiError, Transfer] =
        ssl match
          case Some(s) => s.write(arr, off, arr.length - off)
          case None    => Net.write(fd, arr, off, arr.length - off)
      def loop(off: Int): Task[Unit] =
        if off >= arr.length then ZIO.unit
        else
          ZIO.suspendSucceed(ZIO.fromEither(push(off))).mapError(_.exception).flatMap {
            case Transfer.Moved(n)    => loop(off + n)
            case Transfer.Blocked(on) => AsyncFd.ready(fd, on).mapError(_.exception) *> loop(off)
            case Transfer.Eof         => ZIO.fail(java.io.IOException("the TLS peer closed before the write finished"))
          }
      loop(0)

  private def park(on: Interest): IO[HttpError, Unit] =
    val park    = AsyncFd.ready(fd, on).mapError(e => HttpError.Io(e.exception))
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
