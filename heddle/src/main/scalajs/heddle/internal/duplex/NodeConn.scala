package heddle.internal.duplex

import heddle.error.HttpError
import heddle.internal.node.{Buffers, NetSocket, TlsSocket}
import java.nio.ByteBuffer
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import zio.*

private[heddle] final class NodeConn(val socket: NetSocket, val alpn: String) extends ByteConn:
  private val incoming                                                     = mutable.ArrayBuffer.empty[Byte]
  private var eof                                                          = false
  private var failed: Option[Throwable]                                    = None
  private var readWaiter: Option[(ByteBuffer, IO[HttpError, Int] => Unit)] = None
  private var readTimeout: Duration                                        = Duration.Infinity
  private var timeoutHandle: Option[js.timers.SetTimeoutHandle]            = None
  private val high                                                         = 64 * 1024
  private val low                                                          = 16 * 1024
  private var paused                                                       = false

  socket.on(
    "data",
    ((data: Uint8Array) =>
      append(data)
      if incoming.size > high && !paused then
        socket.pause()
        paused = true
      wakeRead()
    ): js.Function1[Uint8Array, Unit],
  )
  socket.on(
    "end",
    (() =>
      eof = true
      wakeRead()
    ): js.Function0[Unit],
  )
  socket.on(
    "error",
    ((err: js.Error) =>
      failed = Some(java.io.IOException(Option(err.message).getOrElse("socket error")))
      eof = true
      wakeRead()
    ): js.Function1[js.Error, Unit],
  )
  socket.on(
    "close",
    (() =>
      eof = true
      wakeRead()
    ): js.Function0[Unit],
  )

  def read(dst: ByteBuffer): IO[HttpError, Int] =
    ZIO.suspendSucceed {
      failed match
        case Some(e)                   => ZIO.fail(HttpError.Io(e))
        case None if incoming.nonEmpty =>
          ZIO.succeed(take(dst))
        case None if eof =>
          ZIO.succeed(-1)
        case None =>
          val timed = readTimeout != Duration.Infinity && readTimeout.toNanos > 0L
          ZIO.async[Any, HttpError, Int] { cb =>
            readWaiter = Some((dst, cb))
            if timed then
              val ms = math.max(1L, readTimeout.toMillis).min(Int.MaxValue.toLong).toInt
              timeoutHandle = Some(js.timers.setTimeout(ms) {
                if readWaiter.nonEmpty then
                  readWaiter = None
                  cb(ZIO.fail(HttpError.Timeout))
              })
          }
    }

  def write(chunk: Chunk[Byte]): Task[Unit] =
    if chunk.isEmpty then ZIO.unit
    else
      ZIO.async[Any, Throwable, Unit] { cb =>
        val u8 = Buffers.toU8(chunk)
        val _  = socket.write(
          u8,
          (err: js.Error) =>
            if err == null || js.isUndefined(err) then cb(ZIO.unit)
            else cb(ZIO.fail(java.io.IOException(Option(err.message).getOrElse("write failed")))),
        )
        ()
      }

  def close: UIO[Unit] =
    ZIO.succeed {
      timeoutHandle.foreach(js.timers.clearTimeout)
      timeoutHandle = None
      socket.destroy()
      ()
    }

  def setReadTimeout(d: Duration): UIO[Unit] =
    ZIO.succeed { readTimeout = d }

  private def append(data: Uint8Array): Unit =
    var i = 0
    while i < data.length do
      incoming += data(i).toByte
      i += 1

  private def take(dst: ByteBuffer): Int =
    val n = math.min(dst.remaining(), incoming.size)
    if n <= 0 then 0
    else
      val arr = dst.array()
      val off = dst.arrayOffset() + dst.position()
      var i   = 0
      while i < n do
        arr(off + i) = incoming(i)
        i += 1
      incoming.remove(0, n)
      dst.position(dst.position() + n)
      if paused && incoming.size <= low then
        socket.resume()
        paused = false
      n
    end if
  end take

  private def wakeRead(): Unit =
    timeoutHandle.foreach(js.timers.clearTimeout)
    timeoutHandle = None
    readWaiter.foreach { (dst, cb) =>
      readWaiter = None
      failed match
        case Some(e)                   => cb(ZIO.fail(HttpError.Io(e)))
        case None if incoming.nonEmpty => cb(ZIO.succeed(take(dst)))
        case None if eof               => cb(ZIO.succeed(-1))
        case None                      => readWaiter = Some((dst, cb))
    }
  end wakeRead
end NodeConn

object NodeConn:
  def of(socket: NetSocket): NodeConn = new NodeConn(socket, "")

  def tls(socket: TlsSocket): NodeConn =
    val proto = socket.alpnProtocol match
      case s: String => s
      case _         => ""
    new NodeConn(socket, proto)
end NodeConn
