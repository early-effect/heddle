package heddle.server

/** A numeric server setting, named as in configuration. */
enum Setting(val key: String):
  case Port                     extends Setting("port")
  case MaxHeaderBytes           extends Setting("maxHeaderBytes")
  case MaxBodyBytes             extends Setting("maxBodyBytes")
  case ChunkSize                extends Setting("chunkSize")
  case MaxConnections           extends Setting("maxConnections")
  case MaxRequestsPerConnection extends Setting("maxRequestsPerConnection")
  case SoBacklog                extends Setting("soBacklog")
  case MaxConcurrentStreams     extends Setting("http2.maxConcurrentStreams")
  case InitialWindowSize        extends Setting("http2.initialWindowSize")
  case MaxFrameSize             extends Setting("http2.maxFrameSize")
  case MaxHeaderListSize        extends Setting("http2.maxHeaderListSize")
  case MaxOutstandingFrames     extends Setting("http2.maxOutstandingFrames")
end Setting

/** A setting outside the range heddle can serve with. Bounds are inclusive. */
final case class OutOfRange(setting: Setting, value: Long, min: Long, max: Long):
  def message: String = s"${setting.key} is $value, not in $min..$max"

private[heddle] object OutOfRange:
  def check(setting: Setting, value: Long, min: Long, max: Long): Option[OutOfRange] =
    Option.when(value < min || value > max)(OutOfRange(setting, value, min, max))
