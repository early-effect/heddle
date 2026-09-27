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
  case Settings(params: Chunk[(Int, Int)])

  /** RFC 9113 §6.5: an acknowledgement carries no parameters. */
  case SettingsAck
  case Ping(opaque: Long)
  case PingAck(opaque: Long)
  case GoAway(lastStreamId: Int, errorCode: Int, debug: Chunk[Byte] = Chunk.empty)
  case WindowUpdate(streamId: Int, increment: Int)
  case Continuation(streamId: Int, headerBlock: Chunk[Byte], endHeaders: Boolean)
  case Priority(streamId: Int, exclusive: Boolean, streamDependency: Int, weight: Int)
  case PushPromise(streamId: Int, promisedId: Int, headerBlock: Chunk[Byte], endHeaders: Boolean)

  /** A frame type this endpoint does not know. RFC 9113 §4.1: the connection ignores it. The codec keeps every field so
    * decoding stays lossless.
    */
  case Unknown(frameType: Int, flags: Int, streamId: Int, payload: Chunk[Byte])
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
