package heddle.sse

import zio.Duration

/** One event. `data` may span lines (each becomes a `data:` line); `event` and `id` are single-line by type. */
final case class ServerSentEvent(
    data: String,
    event: Option[SseField] = None,
    id: Option[SseField] = None,
    retry: Option[Duration] = None,
)

object ServerSentEvent:
  /** Encoded as a comment (`: ping\\n\\n`). */
  val Heartbeat: ServerSentEvent = ServerSentEvent("")
