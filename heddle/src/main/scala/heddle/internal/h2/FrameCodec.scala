package heddle.internal.h2

import zio.Chunk

private[heddle] object FrameCodec:
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

  def decode(bytes: Chunk[Byte], maxFrame: Int): Either[String, (H2Frame, Chunk[Byte])] =
    if bytes.length < 9 then Left("short header")
    else
      val len = ((bytes(0) & 0xff) << 16) | ((bytes(1) & 0xff) << 8) | (bytes(2) & 0xff)
      if len > maxFrame then Left("frame too large")
      else if bytes.length < 9 + len then Left("short payload")
      else
        val tpe      = bytes(3) & 0xff
        val flags    = bytes(4) & 0xff
        val streamId =
          ((bytes(5) & 0x7f) << 24) | ((bytes(6) & 0xff) << 16) | ((bytes(7) & 0xff) << 8) | (bytes(8) & 0xff)
        val payload = bytes.drop(9).take(len)
        val rest    = bytes.drop(9 + len)
        body(tpe, flags, streamId, payload).map(_ -> rest)

  private def parts(frame: H2Frame): (Int, Int, Int, Chunk[Byte]) =
    frame match
      case H2Frame.Data(id, data, end, pad) =>
        var flags = if end then 0x01 else 0
        val pay   =
          if pad > 0 then
            flags |= 0x08
            Chunk.single(pad.toByte) ++ data ++ Chunk.fromArray(Array.ofDim[Byte](pad))
          else data
        (H2Frame.DataType, flags, id, pay)
      case H2Frame.Headers(id, block, endStream, endHeaders, exclusive, dep, weight) =>
        var flags = 0
        if endStream then flags |= 0x01
        if endHeaders then flags |= 0x04
        val pay =
          if dep != 0 || exclusive then
            flags |= 0x20
            val d = (if exclusive then 0x80000000 else 0) | (dep & 0x7fffffff)
            val p = Array.ofDim[Byte](5)
            p(0) = ((d >>> 24) & 0xff).toByte
            p(1) = ((d >>> 16) & 0xff).toByte
            p(2) = ((d >>> 8) & 0xff).toByte
            p(3) = (d & 0xff).toByte
            p(4) = (weight - 1).toByte
            Chunk.fromArray(p) ++ block
          else block
        (H2Frame.HeadersType, flags, id, pay)
      case H2Frame.RstStream(id, code) =>
        (H2Frame.RstStreamType, 0, id, u32(code))
      case H2Frame.Settings(ack, params) =>
        val flags = if ack then 0x01 else 0
        val pay   =
          params.foldLeft(Chunk.empty[Byte]) { case (acc, (k, v)) =>
            acc ++ u16(k) ++ u32(v)
          }
        (H2Frame.SettingsType, flags, 0, pay)
      case H2Frame.Ping(ack, opaque) =>
        val flags = if ack then 0x01 else 0
        val p     = opaque.toArray.padTo(8, 0.toByte).take(8)
        (H2Frame.PingType, flags, 0, Chunk.fromArray(p))
      case H2Frame.GoAway(last, code, debug) =>
        (H2Frame.GoAwayType, 0, 0, u32(last) ++ u32(code) ++ debug)
      case H2Frame.WindowUpdate(id, inc) =>
        (H2Frame.WindowUpdateType, 0, id, u32(inc))
      case H2Frame.Continuation(id, block, endHeaders) =>
        val flags = if endHeaders then 0x04 else 0
        (H2Frame.ContinuationType, flags, id, block)
      case H2Frame.Priority(id, exclusive, dep, weight) =>
        val d = (if exclusive then 0x80000000 else 0) | (dep & 0x7fffffff)
        val p = Array.ofDim[Byte](5)
        p(0) = ((d >>> 24) & 0xff).toByte
        p(1) = ((d >>> 16) & 0xff).toByte
        p(2) = ((d >>> 8) & 0xff).toByte
        p(3) = (d & 0xff).toByte
        p(4) = (weight - 1).toByte
        (H2Frame.PriorityType, 0, id, Chunk.fromArray(p))
      case H2Frame.PushPromise(id, promised, block, endHeaders) =>
        val flags = if endHeaders then 0x04 else 0
        (H2Frame.PushPromiseType, flags, id, u32(promised) ++ block)

  private def body(tpe: Int, flags: Int, id: Int, payload: Chunk[Byte]): Either[String, H2Frame] =
    tpe match
      case H2Frame.DataType =>
        val (data, pad) = stripPad(flags, payload)
        Right(H2Frame.Data(id, data, (flags & 0x01) != 0, pad))
      case H2Frame.HeadersType =>
        var rest = payload
        var excl = false
        var dep  = 0
        var w    = 16
        if (flags & 0x08) != 0 then rest = rest.drop(1)
        if (flags & 0x20) != 0 && rest.length >= 5 then
          val v = u32val(rest)
          excl = (v & 0x80000000) != 0
          dep = v & 0x7fffffff
          w = (rest(4) & 0xff) + 1
          rest = rest.drop(5)
        if (flags & 0x08) != 0 && payload.nonEmpty then
          val pad = payload(0) & 0xff
          rest = rest.dropRight(pad)
        Right(H2Frame.Headers(id, rest, (flags & 0x01) != 0, (flags & 0x04) != 0, excl, dep, w))
      case H2Frame.RstStreamType if payload.length == 4 =>
        Right(H2Frame.RstStream(id, u32val(payload)))
      case H2Frame.SettingsType =>
        if payload.length % 6 != 0 then Left("bad settings")
        else
          val ps = Chunk.fromIterator(
            payload.toArray.grouped(6).map { g =>
              val k = ((g(0) & 0xff) << 8) | (g(1) & 0xff)
              val v = ((g(2) & 0xff) << 24) | ((g(3) & 0xff) << 16) | ((g(4) & 0xff) << 8) | (g(5) & 0xff)
              k -> v
            }
          )
          Right(H2Frame.Settings((flags & 0x01) != 0, ps))
      case H2Frame.PingType if payload.length == 8 =>
        Right(H2Frame.Ping((flags & 0x01) != 0, payload))
      case H2Frame.GoAwayType if payload.length >= 8 =>
        Right(H2Frame.GoAway(u32val(payload), u32val(payload.drop(4)), payload.drop(8)))
      case H2Frame.WindowUpdateType if payload.length == 4 =>
        Right(H2Frame.WindowUpdate(id, u32val(payload) & 0x7fffffff))
      case H2Frame.ContinuationType =>
        Right(H2Frame.Continuation(id, payload, (flags & 0x04) != 0))
      case H2Frame.PriorityType if payload.length == 5 =>
        val v = u32val(payload)
        Right(H2Frame.Priority(id, (v & 0x80000000) != 0, v & 0x7fffffff, (payload(4) & 0xff) + 1))
      case other => Left(s"unsupported frame $other")

  private def stripPad(flags: Int, payload: Chunk[Byte]): (Chunk[Byte], Int) =
    if (flags & 0x08) == 0 || payload.isEmpty then (payload, 0)
    else
      val pad = payload(0) & 0xff
      (payload.drop(1).dropRight(pad), pad)

  private def u16(n: Int): Chunk[Byte] =
    Chunk((n >>> 8).toByte, n.toByte)

  private def u32(n: Int): Chunk[Byte] =
    Chunk(((n >>> 24) & 0xff).toByte, ((n >>> 16) & 0xff).toByte, ((n >>> 8) & 0xff).toByte, (n & 0xff).toByte)

  private def u32val(c: Chunk[Byte]): Int =
    ((c(0) & 0xff) << 24) | ((c(1) & 0xff) << 16) | ((c(2) & 0xff) << 8) | (c(3) & 0xff)
end FrameCodec
