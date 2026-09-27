package heddle.mcp.protocol

import zio.json.JsonCodec

/** An MCP protocol revision (`2026-07-28`). Open: a peer may name one this library does not know. */
opaque type ProtocolVersion = String

object ProtocolVersion:
  /** Stateless: `server/discover`, per-request `_meta`. */
  val Current: ProtocolVersion = "2026-07-28"

  /** Sessions: `initialize`, `Mcp-Session-Id`. */
  val Legacy: ProtocolVersion = "2025-11-25"

  def apply(raw: String): ProtocolVersion = raw

  extension (v: ProtocolVersion) def value: String = v

  given JsonCodec[ProtocolVersion] = JsonCodec.string
end ProtocolVersion
