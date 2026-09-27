package heddle.internal.h2

import heddle.error.WireError

import zio.Chunk

/** RFC 9113 §4.1 framing. `decode` is total and lossless: `decode(encode(f) ++ rest) == Right(f -> rest)`. */
private[heddle] object FrameCodec:
  private val EndStream    = 0x01
  private val Ack          = 0x01
  private val EndHeaders   = 0x04
  private val Padded       = 0x08
  private val PriorityFlag = 0x20

  def encode(frame: H2Frame): Chunk[Byte] =
    val (tpe, flags, streamId, payload) = parts(frame)
    val len                             = payload.length
    val head                            = Array.ofDim[Byte](9)
    head(0) = ((len >> 16) & 0xff).toByte
    head(1) = ((len >> 8) & 0xff).toByte
    head(2) = (len & 0xff).toByte
    head(3) = tpe.toByte
    head(4) = flags.toByte
    head(5) = ((streamId >>> 24) & 0x7f).toByte
    head(6) = ((streamId >>> 16) & 0xff).toByte
    head(7) = ((streamId >>> 8) & 0xff).toByte
    head(8) = (streamId & 0xff).toByte
    Chunk.fromArray(head) ++ payload
  end encode

  def decode(bytes: Chunk[Byte], maxFrame: Int): Either[WireError, (H2Frame, Chunk[Byte])] =
    if bytes.length < 9 then Left(WireError.TruncatedFrame)
    else
      val len = ((bytes(0) & 0xff) << 16) | ((bytes(1) & 0xff) << 8) | (bytes(2) & 0xff)
      if len > maxFrame then Left(WireError.FrameTooLarge(len, maxFrame))
      else if bytes.length < 9 + len then Left(WireError.TruncatedFrame)
      else
        val streamId = u32val(bytes, 5) & 0x7fffffff
        body(bytes(3) & 0xff, bytes(4) & 0xff, streamId, bytes.slice(9, 9 + len)).map(_ -> bytes.drop(9 + len))

  private def parts(frame: H2Frame): (Int, Int, Int, Chunk[Byte]) =
    frame match
      case H2Frame.Data(id, data, end, pad) =>
        val padded = pad > 0
        val body   = if padded then Chunk.single(pad.toByte) ++ data ++ Chunk.fill(pad)(0.toByte) else data
        (H2Frame.DataType, flag(end, EndStream) | flag(padded, Padded), id, body)
      case H2Frame.Headers(id, block, endStream, endHeaders, exclusive, dep, weight) =>
        val prioritized = exclusive || dep != 0 || weight != 16
        val flags       = flag(endStream, EndStream) | flag(endHeaders, EndHeaders) | flag(prioritized, PriorityFlag)
        (H2Frame.HeadersType, flags, id, if prioritized then priority(exclusive, dep, weight) ++ block else block)
      case H2Frame.RstStream(id, code) =>
        (H2Frame.RstStreamType, 0, id, u32(code))
      case H2Frame.Settings(params) =>
        (H2Frame.SettingsType, 0, 0, params.flatMap((k, v) => u16(k) ++ u32(v)))
      case H2Frame.SettingsAck =>
        (H2Frame.SettingsType, Ack, 0, Chunk.empty)
      case H2Frame.Ping(opaque) =>
        (H2Frame.PingType, 0, 0, u32((opaque >>> 32).toInt) ++ u32(opaque.toInt))
      case H2Frame.PingAck(opaque) =>
        (H2Frame.PingType, Ack, 0, u32((opaque >>> 32).toInt) ++ u32(opaque.toInt))
      case H2Frame.GoAway(last, code, debug) =>
        (H2Frame.GoAwayType, 0, 0, u32(last) ++ u32(code) ++ debug)
      case H2Frame.WindowUpdate(id, inc) =>
        (H2Frame.WindowUpdateType, 0, id, u32(inc))
      case H2Frame.Continuation(id, block, endHeaders) =>
        (H2Frame.ContinuationType, flag(endHeaders, EndHeaders), id, block)
      case H2Frame.Priority(id, exclusive, dep, weight) =>
        (H2Frame.PriorityType, 0, id, priority(exclusive, dep, weight))
      case H2Frame.PushPromise(id, promised, block, endHeaders) =>
        (H2Frame.PushPromiseType, flag(endHeaders, EndHeaders), id, u32(promised) ++ block)
      case H2Frame.Unknown(tpe, flags, id, payload) =>
        (tpe, flags, id, payload)

  private def body(tpe: Int, flags: Int, id: Int, payload: Chunk[Byte]): Either[WireError, H2Frame] =
    def set(bit: Int): Boolean                                       = (flags & bit) != 0
    def sized(n: Int)(frame: => H2Frame): Either[WireError, H2Frame] =
      if payload.length == n then Right(frame) else Left(WireError.FrameSize(tpe, payload.length))
    def atLeast(n: Int, c: Chunk[Byte])(frame: => H2Frame): Either[WireError, H2Frame] =
      if c.length >= n then Right(frame) else Left(WireError.FrameSize(tpe, payload.length))
    tpe match
      case H2Frame.DataType =>
        unpad(tpe, flags, payload).map((data, pad) => H2Frame.Data(id, data, set(EndStream), pad))
      case H2Frame.HeadersType =>
        unpad(tpe, flags, payload).flatMap { (rest, _) =>
          if !set(PriorityFlag) then Right(H2Frame.Headers(id, rest, set(EndStream), set(EndHeaders)))
          else
            atLeast(5, rest) {
              val (exclusive, dep, weight) = priorityOf(rest)
              H2Frame.Headers(id, rest.drop(5), set(EndStream), set(EndHeaders), exclusive, dep, weight)
            }
        }
      case H2Frame.PriorityType =>
        sized(5) {
          val (exclusive, dep, weight) = priorityOf(payload)
          H2Frame.Priority(id, exclusive, dep, weight)
        }
      case H2Frame.RstStreamType =>
        sized(4)(H2Frame.RstStream(id, u32val(payload, 0)))
      case H2Frame.SettingsType if set(Ack) =>
        sized(0)(H2Frame.SettingsAck)
      case H2Frame.SettingsType if payload.length % 6 != 0 =>
        Left(WireError.FrameSize(tpe, payload.length))
      case H2Frame.SettingsType =>
        val params = (0 until payload.length by 6).map(at => u16val(payload, at) -> u32val(payload, at + 2))
        Right(H2Frame.Settings(Chunk.fromIterable(params)))
      case H2Frame.PushPromiseType =>
        unpad(tpe, flags, payload).flatMap { (rest, _) =>
          atLeast(4, rest)(H2Frame.PushPromise(id, u32val(rest, 0) & 0x7fffffff, rest.drop(4), set(EndHeaders)))
        }
      case H2Frame.PingType =>
        sized(8) {
          val opaque = (u32val(payload, 0).toLong << 32) | (u32val(payload, 4).toLong & 0xffffffffL)
          if set(Ack) then H2Frame.PingAck(opaque) else H2Frame.Ping(opaque)
        }
      case H2Frame.GoAwayType =>
        atLeast(8, payload)(H2Frame.GoAway(u32val(payload, 0) & 0x7fffffff, u32val(payload, 4), payload.drop(8)))
      case H2Frame.WindowUpdateType =>
        sized(4)(H2Frame.WindowUpdate(id, u32val(payload, 0) & 0x7fffffff))
      case H2Frame.ContinuationType =>
        Right(H2Frame.Continuation(id, payload, set(EndHeaders)))
      case other =>
        Right(H2Frame.Unknown(other, flags, id, payload))
    end match
  end body

  /** RFC 9113 §6.1: padding as long as the payload or longer is a connection error. */
  private def unpad(tpe: Int, flags: Int, payload: Chunk[Byte]): Either[WireError, (Chunk[Byte], Int)] =
    if (flags & Padded) == 0 then Right(payload -> 0)
    else
      payload.headOption match
        case None       => Left(WireError.FrameSize(tpe, 0))
        case Some(byte) =>
          val pad = byte & 0xff
          if pad >= payload.length then Left(WireError.BadPadding(pad, payload.length))
          else Right(payload.slice(1, payload.length - pad) -> pad)

  private def flag(on: Boolean, bit: Int): Int = if on then bit else 0

  private def priority(exclusive: Boolean, dep: Int, weight: Int): Chunk[Byte] =
    u32(flag(exclusive, 0x80000000) | (dep & 0x7fffffff)) :+ (weight - 1).toByte

  private def priorityOf(c: Chunk[Byte]): (Boolean, Int, Int) =
    val v = u32val(c, 0)
    ((v & 0x80000000) != 0, v & 0x7fffffff, (c(4) & 0xff) + 1)

  private def u16(n: Int): Chunk[Byte] =
    Chunk((n >>> 8).toByte, n.toByte)

  private def u32(n: Int): Chunk[Byte] =
    Chunk(((n >>> 24) & 0xff).toByte, ((n >>> 16) & 0xff).toByte, ((n >>> 8) & 0xff).toByte, (n & 0xff).toByte)

  private def u16val(c: Chunk[Byte], at: Int): Int =
    ((c(at) & 0xff) << 8) | (c(at + 1) & 0xff)

  private def u32val(c: Chunk[Byte], at: Int): Int =
    ((c(at) & 0xff) << 24) | ((c(at + 1) & 0xff) << 16) | ((c(at + 2) & 0xff) << 8) | (c(at + 3) & 0xff)
end FrameCodec
