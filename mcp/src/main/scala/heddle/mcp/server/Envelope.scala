package heddle.mcp.server

import heddle.mcp.protocol.{Implementation, ProtocolVersion, RequestMeta}
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** The server's result envelopes: 2026-07-28 marks results `complete` and names the server in `_meta`; 2025-11-25
  * sessions strip both.
  */
private[mcp] object Envelope:
  val SessionHeader = "Mcp-Session-Id"

  def complete(fields: (String, Json)*): Json.Obj =
    Json.Obj(Chunk.fromIterable(("resultType" -> Json.Str("complete")) +: fields))

  def complete(body: Json.Obj): Json.Obj =
    Json.Obj(("resultType" -> Json.Str("complete")) +: body.fields)

  def withServer(result: Json.Obj, server: Implementation): Json.Obj =
    val info = server.toJsonAST.getOrElse(Json.Obj())
    Json.Obj(result.fields :+ ("_meta" -> Json.Obj(RequestMeta.ServerInfoKey -> info)))

  /** The 2025-11-25 `initialize` result. Capabilities are the same set `server/discover` advertises. */
  def initialize(server: Implementation, capabilities: Json.Obj, instructions: Option[String]): Json.Obj =
    Json.Obj(
      Chunk(
        "protocolVersion" -> Json.Str(ProtocolVersion.Legacy.value),
        "capabilities"    -> capabilities,
        "serverInfo"      -> server.toJsonAST.getOrElse(Json.Obj()),
      ) ++ Chunk.fromIterable(instructions.map("instructions" -> Json.Str(_)))
    )

  /** A 2026 result as a 2025 client expects it: no `resultType`, cache hints, or `_meta`. */
  def forSession(result: Json.Obj): Json.Obj =
    Json.Obj(result.fields.filterNot((k, _) => k == "resultType" || k == "ttlMs" || k == "cacheScope" || k == "_meta"))
end Envelope
