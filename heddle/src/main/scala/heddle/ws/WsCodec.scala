package heddle.ws

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import zio.Chunk

private[heddle] object WsCodec:
  val Guid            = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  val MaxPayload: Int = 1 << 20

  def acceptKey(secWebSocketKey: String): String =
    val md  = MessageDigest.getInstance("SHA-1")
    val sum = md.digest((secWebSocketKey + Guid).getBytes(StandardCharsets.US_ASCII))
    Base64.getEncoder.encodeToString(sum)

  def encode(frame: WebSocketFrame): Chunk[Byte] =
    val (opcode, payload) = frame match
      case WebSocketFrame.Text(t)        => (1, t.getBytes(StandardCharsets.UTF_8))
      case WebSocketFrame.Binary(b)      => (2, b.toArray)
      case WebSocketFrame.Ping(b)        => (9, b.toArray)
      case WebSocketFrame.Pong(b)        => (10, b.toArray)
      case WebSocketFrame.Close(code, r) =>
        val reason = r.getBytes(StandardCharsets.UTF_8)
        val p      = Array.ofDim[Byte](2 + reason.length)
        p(0) = ((code >> 8) & 0xff).toByte
        p(1) = (code & 0xff).toByte
        System.arraycopy(reason, 0, p, 2, reason.length)
        (8, p)
    frameBytes(opcode, payload, fin = true, mask = None)
  end encode

  def frameBytes(opcode: Int, payload: Array[Byte], fin: Boolean, mask: Option[Array[Byte]]): Chunk[Byte] =
    val len    = payload.length
    val b0     = (if fin then 0x80 else 0) | (opcode & 0x0f)
    val masked = if mask.isDefined then 0x80 else 0
    val header = Array.newBuilder[Byte]
    header += b0.toByte
    if len < 126 then header += (masked | len).toByte
    else if len <= 0xffff then
      header += (masked | 126).toByte
      header += ((len >> 8) & 0xff).toByte
      header += (len & 0xff).toByte
    else
      header += (masked | 127).toByte
      var s = 56
      while s >= 0 do
        header += ((len.toLong >> s) & 0xff).toByte
        s -= 8
    end if
    mask.foreach { m =>
      var i = 0
      while i < 4 do
        header += m(i)
        i += 1
    }
    val pay =
      mask match
        case None    => payload
        case Some(m) =>
          val out = Array.ofDim[Byte](len)
          var i   = 0
          while i < len do
            out(i) = (payload(i) ^ m(i % 4)).toByte
            i += 1
          out
    Chunk.fromArray(header.result() ++ pay)
  end frameBytes
end WsCodec
