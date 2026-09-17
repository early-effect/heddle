package heddle.mcp.protocol

import heddle.endpoint.{BoundOp, EndpointDoc, Hint, OpArgs, SchemaJson}
import heddle.http.header.Headers
import heddle.mcp.protocol.JsonRpc.*
import zio.json.ast.Json
import zio.json.{DecoderOps, EncoderOps}
import zio.{Chunk, ZIO}

final case class ToolDef(
    name: String,
    description: String,
    inputSchema: Json,
    outputSchema: Option[Json],
    annotations: Json.Obj,
)

trait ToolCall[-R]:
  def name: String
  def definition: ToolDef
  def call(args: Json, headers: Headers): ZIO[R, Nothing, CallResult]

enum CallResult:
  case Ok(structured: Json, text: String)
  case Failed(text: String)

final class Engine[-R](
    val serverName: String,
    val serverVersion: String,
    val tools: Chunk[ToolCall[R]],
    val instructions: Option[String],
    val listTtlMs: Long,
):
  private val byName: Map[String, ToolCall[R]] =
    tools.map(t => t.name -> t).toMap

  def handle(raw: Json, headers: Headers): ZIO[R, Nothing, Option[Json.Obj]] =
    handleRaw(raw, headers, requireVersion = true)

  def handleCompat(raw: Json, headers: Headers): ZIO[R, Nothing, Option[Json.Obj]] =
    handleRaw(raw, headers, requireVersion = false)

  private def handleRaw(raw: Json, headers: Headers, requireVersion: Boolean): ZIO[R, Nothing, Option[Json.Obj]] =
    raw match
      case msg: Json.Obj => handleObj(msg, headers, requireVersion)
      case _             => ZIO.succeed(Some(error(Json.Null, ParseError, "Parse error")))

  private def handleObj(
      msg: Json.Obj,
      headers: Headers,
      requireVersion: Boolean,
  ): ZIO[R, Nothing, Option[Json.Obj]] =
    val id = parseId(msg)
    methodOf(msg) match
      case None =>
        ZIO.succeed(Some(error(id, InvalidRequest, "Invalid Request")))
      case Some(method) if method.startsWith("notifications/") =>
        ZIO.succeed(None)
      case Some(method) =>
        val params = paramsOf(msg)
        dispatch(id, method, params, headers, requireVersion).map(Some(_))
  end handleObj

  private def dispatch(
      id: Json,
      method: String,
      params: Json.Obj,
      headers: Headers,
      requireVersion: Boolean,
  ): ZIO[R, Nothing, Json.Obj] =
    def run: ZIO[R, Nothing, Json.Obj] =
      method match
        case "server/discover" if !requireVersion =>
          ZIO.succeed(error(id, MethodNotFound, s"Method not found: $method"))
        case "server/discover" => ZIO.succeed(result(id, discover))
        case "ping"            => ZIO.succeed(result(id, wrap(complete())))
        case "tools/list"      => ZIO.succeed(result(id, listTools))
        case "tools/call"      => callTool(id, params, headers)
        case _                 => ZIO.succeed(error(id, MethodNotFound, s"Method not found: $method"))
    if !requireVersion then run
    else
      protocolVersion(params) match
        case None =>
          ZIO.succeed(error(id, InvalidParams, "missing protocol version"))
        case Some(ver) if ver != ProtocolVersion =>
          ZIO.succeed(
            error(
              id,
              UnsupportedVersion,
              "Unsupported protocol version",
              Some(obj("supported" -> Json.Arr(Json.Str(ProtocolVersion)), "requested" -> Json.Str(ver))),
            )
          )
        case Some(_) => run
    end if
  end dispatch

  private def discover: Json.Obj =
    val caps  = obj("tools" -> obj())
    val extra = instructions.fold(List.empty[(String, Json)])(s => List("instructions" -> Json.Str(s)))
    val body  = complete(
      (List(
        "supportedVersions" -> Json.Arr(Json.Str(ProtocolVersion)),
        "capabilities"      -> caps,
        "ttlMs"             -> Json.Num(listTtlMs),
        "cacheScope"        -> Json.Str("public"),
      ) ++ extra)*
    )
    wrap(body)
  end discover

  private def listTools: Json.Obj =
    wrap(
      complete(
        "tools"      -> Json.Arr(tools.map(t => toolJson(t.definition))),
        "ttlMs"      -> Json.Num(listTtlMs),
        "cacheScope" -> Json.Str("public"),
      )
    )

  private def callTool(id: Json, params: Json.Obj, headers: Headers): ZIO[R, Nothing, Json.Obj] =
    val name = params
      .get("name")
      .flatMap:
        case Json.Str(n) => Some(n)
        case _           => None
    val args = params.get("arguments") match
      case Some(o: Json.Obj) => o
      case Some(other)       => other
      case None              => obj()
    name match
      case None    => ZIO.succeed(error(id, InvalidParams, "missing tool name"))
      case Some(n) =>
        byName.get(n) match
          case None       => ZIO.succeed(error(id, InvalidParams, s"Unknown tool: $n"))
          case Some(tool) =>
            tool
              .call(args, headers)
              .map:
                case CallResult.Ok(structured, text) =>
                  result(id, wrap(toolResult(text, structured, isError = false)))
                case CallResult.Failed(text) =>
                  result(id, wrap(toolResult(text, Json.Null, isError = true)))
    end match
  end callTool

  private def toolResult(text: String, structured: Json, isError: Boolean): Json.Obj =
    val content = Json.Arr(obj("type" -> Json.Str("text"), "text" -> Json.Str(text)))
    val base    =
      complete("content" -> content, "isError" -> Json.Bool(isError))
    structured match
      case Json.Null if isError => base
      case Json.Null            => base
      case other                => obj((base.fields.toList :+ ("structuredContent" -> other))*)

  private def toolJson(d: ToolDef): Json =
    val fields =
      List(
        "name"        -> Json.Str(d.name),
        "description" -> Json.Str(d.description),
        "inputSchema" -> d.inputSchema,
      ) ++ d.outputSchema.map(s => "outputSchema" -> s).toList ++
        (if d.annotations.fields.isEmpty then Nil else List("annotations" -> d.annotations))
    obj(fields*)

  private def wrap(body: Json.Obj): Json.Obj =
    withServer(body, serverName, serverVersion)
end Engine

object Engine:
  def bound[R, In, Err, Out](op: BoundOp[R, In, Err, Out]): Either[String, ToolCall[R]] =
    val doc = op.endpoint.doc
    OpArgs.inputSchema(doc).map { schema =>
      val outSchema = successSchema(doc).map(SchemaJson.render(_))
      val defn      = ToolDef(
        name = doc.toolName,
        description = doc.description.orElse(doc.summary).getOrElse(""),
        inputSchema = SchemaJson.render(schema),
        outputSchema = outSchema,
        annotations = hintsJson(doc.hints),
      )
      new ToolCall[R]:
        def name: String                                                    = defn.name
        def definition: ToolDef                                             = defn
        def call(args: Json, headers: Headers): ZIO[R, Nothing, CallResult] =
          runBound(op, args, headers)
    }
  end bound

  def runBound[R, In, Err, Out](
      op: BoundOp[R, In, Err, Out],
      args: Json,
      headers: Headers,
  ): ZIO[R, Nothing, CallResult] =
    OpArgs.request(op.endpoint.doc, args, headers) match
      case Left(msg)  => ZIO.succeed(CallResult.Failed(msg))
      case Right(req) =>
        op.input(req)
          .foldZIO(
            res => ZIO.succeed(CallResult.Failed(res.body.asString)),
            in =>
              op.run(in)
                .fold(
                  err => CallResult.Failed(encodeErr(op, err)),
                  out =>
                    val json = encodeOut(op, out)
                    val text = json match
                      case Json.Str(s) => s
                      case other       => other.toJson
                    CallResult.Ok(json, text),
                ),
          )

  private def encodeOut[R, In, Err, Out](op: BoundOp[R, In, Err, Out], out: Out): Json =
    op.endpoint.outputCodec match
      case Some(c) =>
        val raw = c.encoder.encodeJson(out).toString
        raw.fromJson[Json].getOrElse(Json.Str(raw))
      case None => Json.Str(out.toString)

  private def encodeErr[R, In, Err, Out](op: BoundOp[R, In, Err, Out], err: Err): String =
    op.endpoint.errorCodec match
      case Some(c) => c.encoder.encodeJson(err).toString
      case None    => err.toString

  private def successSchema(doc: EndpointDoc): Option[heddle.endpoint.SchemaDoc] =
    doc.responses.find(r => r.status.code >= 200 && r.status.code < 300).flatMap(_.schema)

  private def hintsJson(hints: List[Hint]): Json.Obj =
    val fields = hints.distinct.collect {
      case Hint.ReadOnly    => "readOnlyHint"    -> Json.Bool(true)
      case Hint.Destructive => "destructiveHint" -> Json.Bool(true)
      case Hint.Idempotent  => "idempotentHint"  -> Json.Bool(true)
      case Hint.OpenWorld   => "openWorldHint"   -> Json.Bool(true)
    }
    obj(fields*)
end Engine
