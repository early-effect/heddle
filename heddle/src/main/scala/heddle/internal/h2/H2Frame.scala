package heddle.internal.h2

import zio.Chunk

private[heddle] enum H2Frame:
  case Data(streamId: Int, data: Chunk[Byte], endStream: Boolean, padding: Int = 0)
  case Headers(
      streamId: Int,
      headerBlock: Chunk[Byte],
      endStream: Boolean,
      endHeaders: Boolean,
      exclusive: Boolean = false,
      streamDependency: Int = 0,
      weight: Int = 16,
  )
  case RstStream(streamId: Int, errorCode: Int)
  case Settings(ack: Boolean, params: Chunk[(Int, Int)])
  case Ping(ack: Boolean, opaque: Chunk[Byte])
  case GoAway(lastStreamId: Int, errorCode: Int, debug: Chunk[Byte] = Chunk.empty)
  case WindowUpdate(streamId: Int, increment: Int)
  case Continuation(streamId: Int, headerBlock: Chunk[Byte], endHeaders: Boolean)
  case Priority(streamId: Int, exclusive: Boolean, streamDependency: Int, weight: Int)
  case PushPromise(streamId: Int, promisedId: Int, headerBlock: Chunk[Byte], endHeaders: Boolean)
end H2Frame

object H2Frame:
  val DataType: Int         = 0
  val HeadersType: Int      = 1
  val PriorityType: Int     = 2
  val RstStreamType: Int    = 3
  val SettingsType: Int     = 4
  val PushPromiseType: Int  = 5
  val PingType: Int         = 6
  val GoAwayType: Int       = 7
  val WindowUpdateType: Int = 8
  val ContinuationType: Int = 9

  val Preface: Array[Byte] =
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n".getBytes(java.nio.charset.StandardCharsets.US_ASCII)

  val DefaultMaxFrameSize: Int = 16384
  val MinMaxFrameSize: Int     = 16384
  val MaxMaxFrameSize: Int     = (1 << 24) - 1
end H2Frame
