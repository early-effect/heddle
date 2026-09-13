package heddle.server

import heddle.internal.h2.H2Frame

final case class Http2Config(
    maxConcurrentStreams: Int = 100,
    initialWindowSize: Int = 65535,
    maxFrameSize: Int = H2Frame.DefaultMaxFrameSize,
    maxHeaderListSize: Int = 8192,
):
  if maxFrameSize < H2Frame.MinMaxFrameSize || maxFrameSize > H2Frame.MaxMaxFrameSize then
    throw IllegalArgumentException(s"maxFrameSize $maxFrameSize not in 16384..16777215")
