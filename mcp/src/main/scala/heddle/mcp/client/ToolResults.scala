package heddle.mcp.client

import java.nio.charset.StandardCharsets
import heddle.http.{Body, Response, Status}
import heddle.mcp.ToolShapes
import heddle.mcp.protocol.{CallToolResult, ContentBlock}
import zio.json.*
import zio.{Chunk, IO, ZIO}

/** A `tools/call` result read back into an endpoint's types: its output, one of its declared errors, or why neither. */
object ToolResults:
  def decode[In, Err, Out](
      ep: heddle.endpoint.Endpoint[In, Err, Out],
      result: CallToolResult,
  ): IO[McpCallFailure[Err], Out] =
    if result.failed then failure(ep, result) else success(ep, result)

  private def failure[Err](
      ep: heddle.endpoint.Endpoint[?, Err, ?],
      result: CallToolResult,
  ): IO[McpCallFailure[Err], Nothing] =
    result.structuredContent.map(sc => ToolShapes.error(ep.doc).unwrap(sc).toJson) match
      case None       => ZIO.fail(McpCallFailure.Failed(text(result)))
      case Some(json) =>
        ep.errors
          .decodeDocument(json)
          .foldZIO(
            reason => ZIO.fail(McpCallFailure.Undecodable(reason)),
            {
              case Some(e) => ZIO.fail(McpCallFailure.Domain(e))
              case None    => ZIO.fail(McpCallFailure.Failed(text(result)))
            },
          )

  /** Structured content is a JSON document. A success that is not JSON has no tool result to read. */
  private def success[Err, Out](
      ep: heddle.endpoint.Endpoint[?, Err, Out],
      result: CallToolResult,
  ): IO[McpCallFailure[Err], Out] =
    ep.outputBody.filter(_.mediaType.isJson) match
      case None =>
        ep.decodeOut(Response.empty(Status.NoContent)).mapError(McpCallFailure.Undecodable(_))
      case Some(codec) =>
        ToolShapes.output(ep.doc) match
          case None        => ZIO.fail(McpCallFailure.NoStructuredContent)
          case Some(shape) =>
            ZIO
              .fromOption(result.structuredContent)
              .orElseFail(McpCallFailure.NoStructuredContent)
              .flatMap { sc =>
                val raw  = shape.unwrap(sc).toJson
                val body = Body.fromBytes(Chunk.fromArray(raw.getBytes(StandardCharsets.UTF_8)), Some(codec.mediaType))
                codec.decode(body).mapError(McpCallFailure.Undecodable(_))
              }

  private def text(result: CallToolResult): String =
    result.content.collect { case ContentBlock.Text(t, _) => t }.mkString("\n")
end ToolResults
