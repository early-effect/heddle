package heddle.mcp.protocol

import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** One entry of `resources/list`. */
final case class Resource(
    uri: String,
    name: String,
    title: Option[String] = None,
    description: Option[String] = None,
    mimeType: Option[String] = None,
    @jsonField("_meta") meta: Option[Json.Obj] = None,
) derives JsonCodec

/** A resource body: `text` or base64 `blob`, told apart by which field is present. */
enum ResourceContents:
  case Text(uri: String, mimeType: Option[String], text: String, meta: Option[Json.Obj])
  case Blob(uri: String, mimeType: Option[String], blob: String, meta: Option[Json.Obj])

  def uri: String
  def mimeType: Option[String]
  def meta: Option[Json.Obj]
end ResourceContents

object ResourceContents:
  private final case class Wire(
      uri: String,
      mimeType: Option[String] = None,
      text: Option[String] = None,
      blob: Option[String] = None,
      @jsonField("_meta") meta: Option[Json.Obj] = None,
  ) derives JsonCodec

  given JsonCodec[ResourceContents] =
    JsonCodec[Wire].transformOrFail(
      w =>
        (w.text, w.blob) match
          case (Some(t), None) => Right(Text(w.uri, w.mimeType, t, w.meta))
          case (None, Some(b)) => Right(Blob(w.uri, w.mimeType, b, w.meta))
          case _               => Left("resource contents need exactly one of text, blob"),
      {
        case Text(u, m, t, meta) => Wire(u, m, text = Some(t), meta = meta)
        case Blob(u, m, b, meta) => Wire(u, m, blob = Some(b), meta = meta)
      },
    )
end ResourceContents

final case class ListResourcesResult(resources: Chunk[Resource], nextCursor: Option[String] = None) derives JsonCodec

final case class ReadResourceResult(contents: Chunk[ResourceContents]) derives JsonCodec
