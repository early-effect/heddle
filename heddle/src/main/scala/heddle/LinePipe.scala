package heddle

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}
import java.nio.charset.StandardCharsets
import zio.{IO, Queue, Task, UIO, ZIO}

trait LinePipe:
  def readLine: IO[Throwable, Option[String]]
  def writeLine(line: String): Task[Unit]

object LinePipe:
  def streams(in: InputStream, out: OutputStream): LinePipe =
    StreamLinePipe(in, out)

  def standard: LinePipe = LinePipePlatform.standard

  /** Two in-memory ends: a line written on one is read on the other. `close` on either end ends the other's reads. */
  def connected: UIO[(Closable, Closable)] =
    for
      aToB <- Queue.unbounded[Option[String]]
      bToA <- Queue.unbounded[Option[String]]
    yield (QueuePipe(read = bToA, write = aToB), QueuePipe(read = aToB, write = bToA))

  /** A pipe whose writer can signal end of input. */
  trait Closable extends LinePipe:
    def close: UIO[Unit]

  private final class QueuePipe(read: Queue[Option[String]], write: Queue[Option[String]]) extends Closable:
    def readLine: IO[Throwable, Option[String]] =
      read.take.tap(line => read.offer(None).when(line.isEmpty))

    def writeLine(line: String): Task[Unit] =
      write.offer(Some(line)).unit

    def close: UIO[Unit] = write.offer(None).unit
end LinePipe

private final class StreamLinePipe(in: InputStream, out: OutputStream) extends LinePipe:
  private val reader = BufferedReader(InputStreamReader(in, StandardCharsets.UTF_8))

  def readLine: IO[Throwable, Option[String]] =
    ZIO.attemptBlocking {
      Option(reader.readLine())
    }

  def writeLine(line: String): Task[Unit] =
    ZIO.attemptBlocking {
      val bytes = (line + "\n").getBytes(StandardCharsets.UTF_8)
      out.write(bytes)
      out.flush()
    }
end StreamLinePipe
