package heddle.mcp.client

import heddle.endpoint.{BodyError, Endpoint}
import heddle.http.{Response, Status}
import heddle.mcp.ToolShapes
import heddle.mcp.protocol.{CallToolResult, ContentBlock}
import zio.{IO, ZIO}
import zio.json.*

/** A `tools/call` result read back into an endpoint's types: its output, one of its declared errors, or why neither. */
object ToolResults:
  def decode[In, Err, Out](ep: Endpoint[In, Err, Out], result: CallToolResult): IO[McpCallFailure[Err], Out] =
    if result.failed then ZIO.fail(failure(ep, result)) else success(ep, result)

  private def failure[Err](ep: Endpoint[?, Err, ?], result: CallToolResult): McpCallFailure[Err] =
    val typed = result.structuredContent
      .map(ToolShapes.error(ep.doc).unwrap)
      .flatMap(json => ep.errors.decodeJson(json.toJson))
    typed match
      case Some(Right(e)) => McpCallFailure.Domain(e)
      case _              => McpCallFailure.Failed(text(result))

  /** An output schema comes only from a JSON output, which always sets `outputCodec`; `ToolShapes` reads both. */
  private def success[Err, Out](ep: Endpoint[?, Err, Out], result: CallToolResult): IO[McpCallFailure[Err], Out] =
    ToolShapes.output(ep.doc).zip(ep.outputCodec) match
      case None =>
        ep.decodeOut(Response.empty(Status.NoContent)).mapError(McpCallFailure.Undecodable(_))
      case Some((shape, codec)) =>
        ZIO
          .fromOption(result.structuredContent)
          .orElseFail(McpCallFailure.NoStructuredContent)
          .flatMap(sc =>
            ZIO
              .fromEither(codec.decoder.decodeJson(shape.unwrap(sc).toJson))
              .mapError(e => McpCallFailure.Undecodable(BodyError.Json(e)))
          )

  private def text(result: CallToolResult): String =
    result.content.collect { case ContentBlock.Text(t, _) => t }.mkString("\n")
end ToolResults
