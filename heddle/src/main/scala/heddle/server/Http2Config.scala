package heddle.server

import zio.Config

final case class Http2Config(
    maxConcurrentStreams: ConcurrentStreams = Http2Config.defaultMaxConcurrentStreams,
    initialWindowSize: WindowSize = Http2Config.defaultInitialWindowSize,
    maxFrameSize: FrameSize = Http2Config.defaultMaxFrameSize,
    maxHeaderListSize: HeaderListSize = Http2Config.defaultMaxHeaderListSize,
    maxOutstandingFrames: OutstandingFrames = Http2Config.defaultMaxOutstandingFrames,
)

object Http2Config:
  val defaultMaxConcurrentStreams: ConcurrentStreams = ConcurrentStreams(100)
  val defaultInitialWindowSize: WindowSize           = WindowSize(65535)
  val defaultMaxFrameSize: FrameSize                 = FrameSize(16 * 1024)
  val defaultMaxHeaderListSize: HeaderListSize       = HeaderListSize(8 * 1024)
  val defaultMaxOutstandingFrames: OutstandingFrames = OutstandingFrames(64)

  val default: Http2Config = Http2Config()

  val descriptor: Config[Http2Config] =
    (
      Configured.int("maxConcurrentStreams", defaultMaxConcurrentStreams, _.toInt, ConcurrentStreams.make) ++
        Configured.int("initialWindowSize", defaultInitialWindowSize, _.toInt, WindowSize.make) ++
        Configured.int("maxFrameSize", defaultMaxFrameSize, _.toInt, FrameSize.make) ++
        Configured.int("maxHeaderListSize", defaultMaxHeaderListSize, _.toInt, HeaderListSize.make) ++
        Configured.int("maxOutstandingFrames", defaultMaxOutstandingFrames, _.toInt, OutstandingFrames.make)
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
