package heddle.ws

import heddle.internal.engine.ConnBuf
import java.nio.charset.StandardCharsets
import zio.*
import zio.stream.ZStream

private[heddle] final class LiveWebSocket(
    src: ConnBuf,
    write: Chunk[Byte] => Task[Unit],
    maxPayload: Int = WsCodec.MaxPayload,
) extends WebSocket:
  def receive: ZStream[Any, Throwable, WebSocketFrame] =
    ZStream
      .repeatZIO(readFrame)
      .mapZIO {
        case p @ WebSocketFrame.Ping(b) => send(WebSocketFrame.Pong(b)).as(p)
        case other                      => ZIO.succeed(other)
      }
      .takeWhile {
        case WebSocketFrame.Close(_, _) => false
        case _                          => true
      }

  def send(frame: WebSocketFrame): Task[Unit] =
    write(WsCodec.encode(frame))

  def close(code: Int = 1000, reason: String = ""): Task[Unit] =
    send(WebSocketFrame.Close(code, reason))

  private def readFrame: Task[WebSocketFrame] =
    src.takeUpTo(2).mapError(e => src.toThrowable(e)).flatMap { hdr =>
      if hdr.length < 2 then ZIO.fail(java.io.IOException("truncated websocket header"))
      else
        val b0     = hdr(0) & 0xff
        val b1     = hdr(1) & 0xff
        val opcode = b0 & 0x0f
        val masked = (b1 & 0x80) != 0
        val len7   = b1 & 0x7f
        lengthOf(len7).flatMap { len =>
          if len > maxPayload then
            write(WsCodec.encode(WebSocketFrame.Close(1009, "too big"))) *>
              ZIO.fail(java.io.IOException("websocket payload too large"))
          else
            val maskIO =
              if masked then src.takeUpTo(4).mapError(e => src.toThrowable(e))
              else ZIO.succeed(Chunk.empty)
            maskIO.flatMap { mask =>
              takeExact(len.toInt).map { payload =>
                val data =
                  if masked && mask.length == 4 then unmask(payload, mask)
                  else payload
                decode(opcode, data)
              }
            }
        }
    }

  private def lengthOf(len7: Int): Task[Long] =
    if len7 < 126 then ZIO.succeed(len7.toLong)
    else if len7 == 126 then takeExact(2).map(c => ((c(0) & 0xff) << 8 | (c(1) & 0xff)).toLong)
    else
      takeExact(8).map { c =>
        var n = 0L
        var i = 0
        while i < 8 do
          n = (n << 8) | (c(i) & 0xff)
          i += 1
        n
      }

  private def takeExact(n: Int): Task[Chunk[Byte]] =
    def go(acc: Chunk[Byte]): Task[Chunk[Byte]] =
      if acc.length >= n then ZIO.succeed(acc.take(n))
      else
        src.takeUpTo((n - acc.length).toLong).mapError(e => src.toThrowable(e)).flatMap { c =>
          if c.isEmpty then ZIO.fail(java.io.IOException("truncated websocket frame"))
          else go(acc ++ c)
        }
    if n == 0 then ZIO.succeed(Chunk.empty) else go(Chunk.empty)

  private def unmask(payload: Chunk[Byte], mask: Chunk[Byte]): Chunk[Byte] =
    val out = Array.ofDim[Byte](payload.length)
    var i   = 0
    while i < payload.length do
      out(i) = (payload(i) ^ mask(i % 4)).toByte
      i += 1
    Chunk.fromArray(out)

  private def decode(opcode: Int, data: Chunk[Byte]): WebSocketFrame =
    opcode match
      case 1 => WebSocketFrame.Text(String(data.toArray, StandardCharsets.UTF_8))
      case 2 => WebSocketFrame.Binary(data)
      case 8 =>
        if data.length >= 2 then
          val code   = ((data(0) & 0xff) << 8) | (data(1) & 0xff)
          val reason = if data.length > 2 then String(data.drop(2).toArray, StandardCharsets.UTF_8) else ""
          WebSocketFrame.Close(code, reason)
        else WebSocketFrame.Close(1000, "")
      case 9  => WebSocketFrame.Ping(data)
      case 10 => WebSocketFrame.Pong(data)
      case _  => WebSocketFrame.Binary(data)
end LiveWebSocket
