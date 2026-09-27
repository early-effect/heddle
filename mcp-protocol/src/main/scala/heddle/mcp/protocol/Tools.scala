package heddle.mcp.protocol

import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** A client or server name and version (`clientInfo`, `serverInfo`). */
final case class Implementation(name: String, version: String, title: Option[String] = None) derives JsonCodec

/** Behaviour hints. Untrusted: a client never grants anything because of them. */
final case class ToolAnnotations(
    title: Option[String] = None,
    readOnlyHint: Option[Boolean] = None,
    destructiveHint: Option[Boolean] = None,
    idempotentHint: Option[Boolean] = None,
    openWorldHint: Option[Boolean] = None,
) derives JsonCodec

/** One entry of `tools/list`. `inputSchema` and `outputSchema` are JSON Schema objects. */
final case class Tool(
    name: ToolName,
    title: Option[String] = None,
    description: Option[String] = None,
    inputSchema: Json.Obj,
    outputSchema: Option[Json.Obj] = None,
    annotations: Option[ToolAnnotations] = None,
    @jsonField("_meta") meta: Option[Json.Obj] = None,
) derives JsonCodec

/** A piece of a tool result or a message. */
@jsonDiscriminator("type")
enum ContentBlock derives JsonCodec:
  @jsonHint("text") case Text(text: String, @jsonField("_meta") meta: Option[Json.Obj] = None)
  @jsonHint("image") case Image(data: String, mimeType: String, @jsonField("_meta") meta: Option[Json.Obj] = None)
  @jsonHint("audio") case Audio(data: String, mimeType: String, @jsonField("_meta") meta: Option[Json.Obj] = None)
  @jsonHint("resource_link") case ResourceLink(
      uri: String,
      name: String,
      mimeType: Option[String] = None,
      description: Option[String] = None,
      @jsonField("_meta") meta: Option[Json.Obj] = None,
  )
  @jsonHint("resource") case Embedded(resource: ResourceContents, @jsonField("_meta") meta: Option[Json.Obj] = None)
end ContentBlock

/** `tools/call`'s result. A tool's own failure is `isError: true` with content, never a JSON-RPC error.
  * `structuredContent` is an object, as the spec requires; it conforms to the tool's `outputSchema`.
  */
final case class CallToolResult(
    content: Chunk[ContentBlock],
    structuredContent: Option[Json.Obj] = None,
    isError: Option[Boolean] = None,
    @jsonField("_meta") meta: Option[Json.Obj] = None,
) derives JsonCodec:
  def failed: Boolean = isError.contains(true)

final case class ListToolsResult(tools: Chunk[Tool], nextCursor: Option[String] = None) derives JsonCodec
