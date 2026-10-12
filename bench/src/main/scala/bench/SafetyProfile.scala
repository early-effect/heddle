package bench

import zio.*

/** One profile for both safety servers. Production defaults are not this profile. */
object SafetyProfile:
  val headerBytes: Int      = 16 * 1024
  val bodyBytes: Long       = 10L * 1024 * 1024
  val readTimeout: Duration = 2.seconds
  val backlog: Int          = 100
  val maxConnections: Int   = 32

  def describe: String =
    s"safety profile headerBytes=$headerBytes bodyBytes=$bodyBytes readTimeout=${readTimeout.toMillis}ms backlog=$backlog tcpNoDelay=true host=127.0.0.1"
end SafetyProfile
