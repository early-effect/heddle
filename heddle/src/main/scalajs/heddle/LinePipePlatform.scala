package heddle

import heddle.internal.node.{Buffers, Process}
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import zio.{Chunk, IO, Task, ZIO}

private[heddle] object LinePipePlatform:
  def standard: LinePipe = ProcessLinePipe

  private object ProcessLinePipe extends LinePipe:
    private val buf                                                   = mutable.ArrayBuffer.empty[Byte]
    private var eof                                                   = false
    private var waiter: Option[IO[Throwable, Option[String]] => Unit] = None

    Process.stdin.on(
      "data",
      ((data: Uint8Array) =>
        var i = 0
        while i < data.length do
          buf += data(i).toByte
          i += 1
        wake()
      ): js.Function1[Uint8Array, Unit],
    )
    Process.stdin.on(
      "end",
      (() =>
        eof = true
        wake()
      ): js.Function0[Unit],
    )

    def readLine: IO[Throwable, Option[String]] =
      ZIO.suspendSucceed {
        takeLine match
          case some @ Some(_) => ZIO.succeed(some)
          case None if eof    => ZIO.succeed(None)
          case None           =>
            ZIO.async[Any, Throwable, Option[String]] { cb =>
              waiter = Some(cb)
            }
      }

    def writeLine(line: String): Task[Unit] =
      ZIO.succeed {
        val bytes = (line + "\n").getBytes(StandardCharsets.UTF_8)
        val _     = Process.stdout.write(Buffers.toU8(Chunk.fromArray(bytes)))
        ()
      }

    private def takeLine: Option[String] =
      val nl = buf.indexOf('\n'.toByte)
      if nl < 0 then None
      else
        val raw = buf.slice(0, nl).toArray
        buf.remove(0, nl + 1)
        val line =
          if raw.nonEmpty && raw.last == '\r' then String(raw, 0, raw.length - 1, StandardCharsets.UTF_8)
          else String(raw, StandardCharsets.UTF_8)
        Some(line)
    end takeLine

    private def wake(): Unit =
      waiter.foreach { cb =>
        takeLine match
          case some @ Some(_) =>
            waiter = None
            cb(ZIO.succeed(some))
          case None if eof =>
            waiter = None
            cb(ZIO.succeed(None))
          case None => ()
      }
  end ProcessLinePipe
end LinePipePlatform
