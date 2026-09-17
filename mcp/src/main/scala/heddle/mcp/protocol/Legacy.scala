package heddle.mcp.protocol

import heddle.mcp.protocol.JsonRpc.*
import zio.json.ast.Json

object Legacy:
  val ProtocolVersion = JsonRpc.LegacyProtocolVersion
  val SessionHeader   = "Mcp-Session-Id"

  def initializeResult(
      id: Json,
      serverName: String,
      serverVersion: String,
      instructions: Option[String],
  ): Json.Obj =
    val extra = instructions.fold(List.empty[(String, Json)])(s => List("instructions" -> Json.Str(s)))
    result(
      id,
      obj(
        (
          List(
            "protocolVersion" -> Json.Str(ProtocolVersion),
            "capabilities"    -> obj("tools" -> obj()),
            "serverInfo"      -> serverInfo(serverName, serverVersion),
          ) ++ extra
        )*
      ),
    )
  end initializeResult

  def stripEnvelope(rpc: Json.Obj): Json.Obj =
    rpc.get("result") match
      case Some(body: Json.Obj) =>
        val kept = body.fields.filterNot { (k, _) =>
          k == "resultType" || k == "ttlMs" || k == "cacheScope" || k == "_meta"
        }
        Json.Obj(rpc.fields.map {
          case ("result", _) => "result" -> Json.Obj(kept)
          case other         => other
        })
      case _ => rpc
end Legacy
