package heddle.client

import heddle.error.{HeddleError, HttpError}

/** Host and port a request connects to. `host` is never bracketed. */
final case class Authority(host: String, port: Int):
  def render: String = if host.contains(':') then s"[$host]:$port" else s"$host:$port"

/** Why an exchange produced no response. A response with any status is a success, not a `ClientError`. */
enum ClientError(val message: String) extends HeddleError:
  case InvalidTarget(target: String, reason: String) extends ClientError(s"Cannot send to '$target': $reason")
  case Connect(authority: Authority, cause: Throwable)
      extends ClientError(s"Connect to ${authority.render} failed: $cause")
  case ConnectTimeout(authority: Authority)        extends ClientError(s"Connect to ${authority.render} timed out")
  case Tls(authority: Authority, cause: Throwable) extends ClientError(s"TLS with ${authority.render} failed: $cause")
  case ReadTimeout(authority: Authority)           extends ClientError(s"${authority.render} stopped answering")
  case Io(authority: Authority, cause: Throwable)  extends ClientError(s"I/O with ${authority.render} failed: $cause")
  case Protocol(authority: Authority, error: HttpError)
      extends ClientError(s"${authority.render} sent an invalid response: ${error.message}")
  case PoolExhausted(authority: Authority) extends ClientError(s"No free connection to ${authority.render}")
  case InvalidTrust(reason: String)        extends ClientError(s"Unusable trust material: $reason")

  /** A streamed response body failed after its head arrived. */
  case BodyFailed(cause: Throwable) extends ClientError(s"Response body failed: $cause")
end ClientError
