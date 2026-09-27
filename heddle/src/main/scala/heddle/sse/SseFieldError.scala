package heddle.sse

import heddle.error.HeddleError

/** Why text cannot be an SSE `event` or `id`. */
enum SseFieldError(val message: String) extends HeddleError:
  case LineBreak(at: Int) extends SseFieldError(s"an SSE field holds no CR or LF, and this has one at $at")
