package heddle.client

import heddle.error.HeddleError
import heddle.http.{Scheme, UrlError}

/** Why a request cannot be sent: where it goes is not an HTTP server the client can reach. */
enum TargetError(val message: String) extends HeddleError:
  case NoHost                     extends TargetError("no host: use an absolute URL or set a Host header")
  case NotHttp(scheme: Scheme)    extends TargetError(s"${scheme.render} is not an HTTP scheme")
  case Malformed(error: UrlError) extends TargetError(error.message)
