package heddle.internal.duplex

import java.nio.ByteBuffer
import heddle.error.HttpError
import zio.*

/** Where a connection's bytes go. A failed write is `HttpError.Io`. */
private[heddle] type Sink = Chunk[Byte] => IO[HttpError, Unit]

private[heddle] trait ByteConn:
  def read(dst: ByteBuffer): IO[HttpError, Int]

  /** The read used when a socket deadline, not a ZIO timeout, unblocks the call. Platforms that already park without
    * shifting keep [[read]].
    */
  def readOnCarrier(dst: ByteBuffer): IO[HttpError, Int] = read(dst)

  def write(chunk: Chunk[Byte]): IO[HttpError, Unit]
  def close: UIO[Unit]
  def setReadTimeout(d: Duration): UIO[Unit]
end ByteConn

/** Why `accept` returned no connection. `Closed` is the listener shutting down, which ends the accept loop. */
private[heddle] enum AcceptError:
  case Closed
  case Failed(cause: Throwable)

/** A listening socket; `C` is the connection its platform's TLS needs, so no one type-tests a `ByteConn`. */
private[heddle] trait Listener[+C <: ByteConn]:
  def localPort: UIO[Int]
  def accept: IO[AcceptError, C]
  def close: UIO[Unit]
