package heddle.sse

import zio.Duration

final case class ServerSentEvent private (
    data: String,
    event: Option[String],
    id: Option[String],
    retry: Option[Duration],
)

object ServerSentEvent:
  /** Encoded as a comment (`: ping\\n\\n`). */
  val Heartbeat: ServerSentEvent = new ServerSentEvent("", None, None, None)

  def apply(
      data: String,
      event: Option[String] = None,
      id: Option[String] = None,
      retry: Option[Duration] = None,
  ): ServerSentEvent =
    event.foreach(forbidCrLf("event", _))
    id.foreach(forbidCrLf("id", _))
    new ServerSentEvent(data, event, id, retry)

  private def forbidCrLf(field: String, value: String): Unit =
    var i = 0
    while i < value.length do
      val c = value.charAt(i)
      if c == '\n' || c == '\r' then throw IllegalArgumentException(s"SSE $field must not contain CR or LF")
      i += 1
end ServerSentEvent
