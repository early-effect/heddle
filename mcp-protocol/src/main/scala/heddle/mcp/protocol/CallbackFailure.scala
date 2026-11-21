package heddle.mcp.protocol

/** Why a webhook callback was refused. The wire value is the only text a client is given. */
enum CallbackFailure(val wire: String):
  case ConnectionRefused extends CallbackFailure("connection_refused")
  case Timeout           extends CallbackFailure("timeout")
  case TlsError          extends CallbackFailure("tls_error")
  case Http4xx           extends CallbackFailure("http_4xx")
  case Http5xx           extends CallbackFailure("http_5xx")
  case ChallengeFailed   extends CallbackFailure("challenge_failed")

object CallbackFailure:
  def fromWire(raw: String): Option[CallbackFailure] =
    CallbackFailure.values.find(_.wire == raw)
