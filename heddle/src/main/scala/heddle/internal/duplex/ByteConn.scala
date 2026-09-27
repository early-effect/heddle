package heddle.internal.duplex

import java.nio.ByteBuffer
import heddle.error.HttpError
import zio.*

private[heddle] trait ByteConn:
  def read(dst: ByteBuffer): IO[HttpError, Int]
  def write(chunk: Chunk[Byte]): Task[Unit]
  def close: UIO[Unit]
  def setReadTimeout(d: Duration): UIO[Unit]

/** Why `accept` returned no connection. `Closed` is the listener shutting down, which ends the accept loop. */
private[heddle] enum AcceptError:
  case Closed
  case Failed(cause: Throwable)

private[heddle] trait Listener:
  def localPort: UIO[Int]
  def accept: IO[AcceptError, ByteConn]
  def close: UIO[Unit]
