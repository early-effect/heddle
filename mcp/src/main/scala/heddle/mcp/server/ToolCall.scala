package heddle.mcp.server

import heddle.endpoint.{BoundOp, EndpointDoc, Hint, OpArgs, SchemaDoc, SchemaJson}
import heddle.http.header.Headers
import heddle.mcp.McpBuildError
import heddle.mcp.protocol.{CallToolResult, ContentBlock, Structured, Tool, ToolAnnotations, ToolName}
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, ZIO}

/** One tool a server lists and runs. Its own failures are `isError` results, never JSON-RPC errors. */
trait ToolCall[-R]:
  def tool: Tool
  def call(args: Json.Obj, headers: Headers): ZIO[R, Nothing, CallToolResult]

object ToolCall:
  def apply[R](t: Tool)(run: (Json.Obj, Headers) => ZIO[R, Nothing, CallToolResult]): ToolCall[R] =
    new ToolCall[R]:
      val tool: Tool                                                              = t
      def call(args: Json.Obj, headers: Headers): ZIO[R, Nothing, CallToolResult] = run(args, headers)

  /** A bound operation as a tool: arguments become the request, the typed output becomes `structuredContent`. */
  def bound[R, In, Err, Out](op: BoundOp[R, In, Err, Out]): Either[McpBuildError, ToolCall[R]] =
    val doc = op.endpoint.doc
    for
      name   <- ToolName.from(doc.toolName).left.map(McpBuildError.InvalidToolName(doc.toolName, _))
      schema <- OpArgs.inputSchema(doc).left.map(McpBuildError.NotPromotable(doc.toolName, _))
      input  <- objectSchema(SchemaJson.render(schema), doc.toolName)
    yield
      val out = successSchema(doc).map(s => Structured.of(SchemaJson.render(s)))
      val err = errorShape(doc)
      val t   = Tool(
        name = name,
        description = doc.description.orElse(doc.summary),
        inputSchema = input,
        outputSchema = out.map(_.outputSchema),
        annotations = annotations(doc.hints),
      )
      ToolCall(t)((args, headers) => runBound(op, args, headers, out, err))
    end for
  end bound

  def runBound[R, In, Err, Out](
      op: BoundOp[R, In, Err, Out],
      args: Json.Obj,
      headers: Headers,
      out: Option[Structured],
      err: Structured,
  ): ZIO[R, Nothing, CallToolResult] =
    OpArgs.request(op.endpoint.doc, args, headers) match
      case Left(msg)  => ZIO.succeed(failed(msg))
      case Right(req) =>
        op.input(req)
          .foldZIO(
            res => res.body.utf8.orElseSucceed(res.status.text).map(failed),
            in =>
              op.run(in)
                .fold(
                  e => typedFailure(op.endpoint.errors.json(e), err),
                  o => success(encode(op, o), out),
                ),
          )

  /** A typed value as a result: the text block carries the same JSON for a model that reads only text. */
  def success(value: Json, shape: Option[Structured]): CallToolResult =
    CallToolResult(Chunk(ContentBlock.Text(text(value))), shape.map(_.wrap(value)))

  /** An endpoint's own error: `isError`, with the error ADT's JSON in `structuredContent` for a typed caller. */
  def typedFailure(errorJson: String, shape: Structured): CallToolResult =
    errorJson.fromJson[Json] match
      case Right(json) =>
        CallToolResult(Chunk(ContentBlock.Text(errorJson)), Some(shape.wrap(json)), isError = Some(true))
      case Left(_) => failed(errorJson)

  def failed(message: String): CallToolResult =
    CallToolResult(Chunk(ContentBlock.Text(message)), isError = Some(true))

  private def text(value: Json): String =
    value match
      case Json.Str(s) => s
      case other       => other.toJson

  private def encode[R, In, Err, Out](op: BoundOp[R, In, Err, Out], out: Out): Json =
    op.endpoint.outputCodec match
      case Some(c) =>
        val raw = c.encoder.encodeJson(out).toString
        raw.fromJson[Json].getOrElse(Json.Str(raw))
      case None => Json.Str(out.toString)

  private[server] def objectSchema(schema: Json, tool: String): Either[McpBuildError, Json.Obj] =
    schema match
      case o: Json.Obj if o.get("type").contains(Json.Str("object")) => Right(o)
      case _ => Left(McpBuildError.NotPromotable(tool, "tool arguments must be a JSON object"))

  private def successSchema(doc: EndpointDoc): Option[SchemaDoc] =
    doc.responses.find(r => r.status.code >= 200 && r.status.code < 300).flatMap(_.schema)

  /** The shape of the endpoint's error JSON, from the schemas its error statuses document. */
  private def errorShape(doc: EndpointDoc): Structured =
    val schemas = doc.responses.filterNot(_.status.isSuccess).flatMap(_.schema).distinct
    val schema  = schemas match
      case one :: Nil => one
      case many       => SchemaDoc.OneOf(None, many)
    Structured.of(SchemaJson.render(schema))

  private def annotations(hints: List[Hint]): Option[ToolAnnotations] =
    if hints.isEmpty then None
    else
      val on = (h: Hint) => Option.when(hints.contains(h))(true)
      Some(
        ToolAnnotations(
          readOnlyHint = on(Hint.ReadOnly),
          destructiveHint = on(Hint.Destructive),
          idempotentHint = on(Hint.Idempotent),
          openWorldHint = on(Hint.OpenWorld),
        )
      )
end ToolCall
