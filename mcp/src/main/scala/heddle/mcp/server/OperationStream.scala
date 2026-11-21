package heddle.mcp.server

import heddle.endpoint.{BoundSubscription, ErrorCodec, OpArgs, OpArgsError, SchemaJson}
import heddle.http.header.Headers
import heddle.mcp.McpBuildError
import heddle.mcp.protocol.{RpcError, ToolName}
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.ZIO

/** One fact on an open subscription, or the terminal failure of that pull. */
private[mcp] enum StreamFact:
  case Event(id: String, data: Json)
  case Failed(data: Option[Json])

/** A stream operation a server can listen to. It is not a tool. */
private[mcp] final class OperationStream[-R](
    val name: ToolName,
    val inputSchema: Json.Obj,
    val elementSchema: Json,
    open: (Json.Obj, Headers) => ZIO[R, RpcError, ZStream[R, Nothing, StreamFact]],
):
  def pull(args: Json.Obj, headers: Headers): ZIO[R, RpcError, ZStream[R, Nothing, StreamFact]] =
    open(args, headers)

object OperationStream:
  def from[R, In, Err, A](bound: BoundSubscription[R, In, Err, A]): Either[McpBuildError, OperationStream[R]] =
    val doc  = bound.subscription.doc
    val tool = doc.toolName
    if doc.promoted then Left(McpBuildError.NotPromotable(tool, OpArgsError.Subscription))
    else
      for
        name   <- ToolName.from(tool).left.map(McpBuildError.InvalidToolName(tool, _))
        schema <- OpArgs.inputSchema(doc).left.map(McpBuildError.NotPromotable(tool, _))
        input  <- ToolCall.objectSchema(SchemaJson.render(schema), tool)
      yield
        val element = SchemaJson.render(bound.subscription.element)
        new OperationStream(name, input, element, (args, headers) => pull(bound, args, headers))

  private def pull[R, In, Err, A](
      bound: BoundSubscription[R, In, Err, A],
      args: Json.Obj,
      headers: Headers,
  ): ZIO[R, RpcError, ZStream[R, Nothing, StreamFact]] =
    val sub = bound.subscription
    OpArgs.request(sub.doc, args, headers) match
      case Left(e)    => ZIO.fail(RpcError.InvalidParams(e.message))
      case Right(req) =>
        bound
          .input(req)
          .foldZIO(
            res => res.body.utf8.orElseSucceed(res.status.text).flatMap(text => ZIO.fail(RpcError.InvalidParams(text))),
            in =>
              bound
                .open(in)
                .mapBoth(
                  err => failed(sub.endpoint.errors, err),
                  stream =>
                    stream
                      .map(item => StreamFact.Event(item.id.value, sub.dataJson(item.value)))
                      .catchAll(err => ZStream.succeed(StreamFact.Failed(jsonData(sub.endpoint.errors, err)))),
                ),
          )

  private def failed[E](errors: ErrorCodec[E], error: E): RpcError =
    RpcError.OperationFailed(jsonData(errors, error))

  private def jsonData[E](errors: ErrorCodec[E], error: E): Option[Json] =
    errors.json(error).flatMap(raw => raw.fromJson[Json].toOption)
end OperationStream
