package heddle.server

import heddle.BytesLength
import heddle.BytesLength.*
import heddle.internal.h2.H2Frame
import zio.Config

final case class Http2Config(
    maxConcurrentStreams: Int = Http2Config.defaultMaxConcurrentStreams,
    initialWindowSize: BytesLength = Http2Config.defaultInitialWindowSize,
    maxFrameSize: BytesLength = Http2Config.defaultMaxFrameSize,
    maxHeaderListSize: BytesLength = Http2Config.defaultMaxHeaderListSize,
    maxOutstandingFrames: Int = Http2Config.defaultMaxOutstandingFrames,
):
  if maxFrameSize.toInt < H2Frame.MinMaxFrameSize || maxFrameSize.toInt > H2Frame.MaxMaxFrameSize then
    throw IllegalArgumentException(s"maxFrameSize $maxFrameSize not in 16384..16777215")
  if maxOutstandingFrames < 1 then throw IllegalArgumentException("maxOutstandingFrames must be >= 1")
end Http2Config

object Http2Config:
  val defaultMaxConcurrentStreams: Int      = 100
  val defaultInitialWindowSize: BytesLength = 64.K - 1.B
  val defaultMaxFrameSize: BytesLength      = 16.K
  val defaultMaxHeaderListSize: BytesLength = 8.K
  val defaultMaxOutstandingFrames: Int      = 64

  val default: Http2Config = Http2Config()

  val descriptor: Config[Http2Config] =
    (
      Config.int("maxConcurrentStreams").withDefault(defaultMaxConcurrentStreams) ++
        Config.long("initialWindowSize").map(BytesLength(_)).withDefault(defaultInitialWindowSize) ++
        Config.long("maxFrameSize").map(BytesLength(_)).withDefault(defaultMaxFrameSize) ++
        Config.long("maxHeaderListSize").map(BytesLength(_)).withDefault(defaultMaxHeaderListSize) ++
        Config.int("maxOutstandingFrames").withDefault(defaultMaxOutstandingFrames)
    ).nested("http2").map {
      (maxConcurrentStreams, initialWindowSize, maxFrameSize, maxHeaderListSize, maxOutstandingFrames) =>
        Http2Config(
          maxConcurrentStreams = maxConcurrentStreams,
          initialWindowSize = initialWindowSize,
          maxFrameSize = maxFrameSize,
          maxHeaderListSize = maxHeaderListSize,
          maxOutstandingFrames = maxOutstandingFrames,
        )
    }
end Http2Config
