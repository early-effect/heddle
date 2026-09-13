package heddle.error

import heddle.http.{Response, Status}
enum HttpError(val message: String) extends HeddleError:
  case HeadersTooLarge        extends HttpError("Request headers too large")
  case BodyTooLarge           extends HttpError("Request body too large")
  case Malformed(msg: String) extends HttpError(msg)
  case Io(cause: Throwable)   extends HttpError(s"HTTP I/O failed: $cause")

  def status: Option[Status] =
    this match
      case HeadersTooLarge => Some(Status.RequestHeaderFieldsTooLarge)
      case BodyTooLarge    => Some(Status.ContentTooLarge)
      case Malformed(_)    => Some(Status.BadRequest)
      case Io(_)           => None

  def toResponse: Option[Response] =
    status.map(s => Response.text(message, s))
end HttpError
