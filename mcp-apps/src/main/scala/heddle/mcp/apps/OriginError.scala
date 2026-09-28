package heddle.mcp.apps

import heddle.error.HeddleError
import zio.json.JsonCodec

/** Why text is not an origin a policy can name: `scheme://host[:port]`, no wildcard, path, or userinfo. */
enum OriginError(val message: String) extends HeddleError derives JsonCodec:
  case NoScheme                      extends OriginError("an origin is scheme://host[:port]")
  case UnknownScheme(scheme: String) extends OriginError(s"$scheme is not https, http, wss, or ws")
  case NotBare(char: Char)           extends OriginError(s"'$char' makes this more than scheme://host[:port]")
  case BadHost(host: String)         extends OriginError(s"'$host' is not a host")
  case BadPort(port: String)         extends OriginError(s"'$port' is not a port from 1 to 65535")
