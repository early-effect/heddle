package heddle

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}
import java.nio.charset.StandardCharsets
import zio.{IO, Task, ZIO}

trait LinePipe:
  def readLine: IO[Throwable, Option[String]]
  def writeLine(line: String): Task[Unit]

object LinePipe:
  def streams(in: InputStream, out: OutputStream): LinePipe =
    StreamLinePipe(in, out)

  def standard: LinePipe = LinePipePlatform.standard

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
