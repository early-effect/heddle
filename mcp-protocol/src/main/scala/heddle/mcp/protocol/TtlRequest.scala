package heddle.mcp.protocol

/** How a subscriber asked for `ttlMs`. Absent omits the field. `Unbounded` is JSON `null`. `Millis` is a number. */
enum TtlRequest:
  case Absent
  case Unbounded
  case Millis(ms: Long)
