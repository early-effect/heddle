package heddle.mcp.protocol

import zio.json.*
import zio.json.ast.Json

/** `notifications/cancelled`. A notification has no error response, so a body that is not a request id plus an optional
  * reason is `None` and the receiver ignores it.
  */
final case class Cancelled(requestId: RequestId, reason: Option[String]):
  def message: Message.Notification =
    Message.Notification(Notifications.Cancelled, Cancelled.params(this))

object Cancelled:
  def params(cancelled: Cancelled): Json.Obj =
    val id = "requestId" -> cancelled.requestId.json
    cancelled.reason match
      case None         => Json.Obj(id)
      case Some(reason) => Json.Obj(id, "reason" -> Json.Str(reason))

  /** Total. A boolean id, a missing id, or a non-string reason is `None`, not a stringly decode error. */
  def from(body: Json.Obj): Option[Cancelled] =
    body.get("requestId").flatMap(RequestId.fromJson).flatMap { id =>
      body.get("reason") match
        case None              => Some(Cancelled(id, None))
        case Some(Json.Str(s)) => Some(Cancelled(id, Some(s)))
        case Some(_)           => None
    }

  given JsonCodec[Cancelled] =
    JsonCodec[Json].transformOrFail(
      {
        case obj: Json.Obj => from(obj).toRight("not a cancellation")
        case _             => Left("not a cancellation")
      },
      params,
    )
end Cancelled
