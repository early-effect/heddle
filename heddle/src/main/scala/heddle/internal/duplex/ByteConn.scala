package heddle.internal.duplex

import java.nio.ByteBuffer
import heddle.error.HttpError
import zio.*

private[heddle] trait ByteConn:
  def read(dst: ByteBuffer): IO[HttpError, Int]
  def write(chunk: Chunk[Byte]): Task[Unit]
  def close: UIO[Unit]
  def setReadTimeout(d: Duration): UIO[Unit]

private[heddle] trait Listener:
  def localPort: UIO[Int]
  def accept: Task[ByteConn]
  def close: UIO[Unit]
