package heddle.mcp.protocol

import zio.Chunk
import zio.json.ast.Json

object JsonRpc:
  val JsonRpcVersion  = "2.0"
  val ProtocolVersion = "2026-07-28"
  val MetaVersion     = "io.modelcontextprotocol/protocolVersion"
  val MetaClientInfo  = "io.modelcontextprotocol/clientInfo"
  val MetaClientCaps  = "io.modelcontextprotocol/clientCapabilities"
  val MetaServerInfo  = "io.modelcontextprotocol/serverInfo"

  val ParseError         = -32700
  val InvalidRequest     = -32600
  val MethodNotFound     = -32601
  val InvalidParams      = -32602
  val InternalError      = -32603
  val HeaderMismatch     = -32020
  val UnsupportedVersion = -32022

  def obj(fields: (String, Json)*): Json.Obj =
    Json.Obj(Chunk.fromIterable(fields))

  def result(id: Json, body: Json.Obj): Json.Obj =
    obj("jsonrpc" -> Json.Str(JsonRpcVersion), "id" -> id, "result" -> body)

  def error(id: Json, code: Int, message: String, data: Option[Json] = None): Json.Obj =
    val err =
      data match
        case None    => obj("code" -> Json.Num(code), "message" -> Json.Str(message))
        case Some(d) => obj("code" -> Json.Num(code), "message" -> Json.Str(message), "data" -> d)
    obj("jsonrpc" -> Json.Str(JsonRpcVersion), "id" -> id, "error" -> err)

  def complete(fields: (String, Json)*): Json.Obj =
    obj(("resultType" -> Json.Str("complete")) +: fields*)

  def serverInfo(name: String, version: String): Json.Obj =
    obj("name" -> Json.Str(name), "version" -> Json.Str(version))

  def withServer(result: Json.Obj, name: String, version: String): Json.Obj =
    val meta = obj(MetaServerInfo -> serverInfo(name, version))
    obj((result.fields.toList :+ ("_meta" -> meta))*)

  def parseId(msg: Json.Obj): Json =
    msg.get("id").getOrElse(Json.Null)

  def methodOf(msg: Json.Obj): Option[String] =
    msg
      .get("method")
      .flatMap:
        case Json.Str(m) => Some(m)
        case _           => None

  def paramsOf(msg: Json.Obj): Json.Obj =
    msg.get("params") match
      case Some(o: Json.Obj) => o
      case _                 => obj()

  def metaOf(params: Json.Obj): Json.Obj =
    params.get("_meta") match
      case Some(o: Json.Obj) => o
      case _                 => obj()

  def protocolVersion(params: Json.Obj): Option[String] =
    metaOf(params)
      .get(MetaVersion)
      .flatMap:
        case Json.Str(v) => Some(v)
        case _           => None
end JsonRpc
