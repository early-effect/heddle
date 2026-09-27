package heddle.mcp.protocol

/** Which handshake a conversation runs under. Servers answer both; clients pick one when they connect. */
enum Era:
  /** 2026-07-28: `server/discover`, a protocol version in every request's `_meta`, `complete` results. */
  case Stateless

  /** 2025-11-25: `initialize`, then a session; results without the 2026 envelope. */
  case Session
