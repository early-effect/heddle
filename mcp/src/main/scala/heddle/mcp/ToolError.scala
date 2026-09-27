package heddle.mcp

import heddle.endpoint.{Schema, SchemaJson}
import heddle.mcp.protocol.Structured
import zio.json.JsonCodec

/** How a native tool's own error travels: as `isError` with the error's JSON in `structuredContent`. A tool whose error
  * type is `Nothing` cannot fail and needs no codec.
  */
trait ToolError[E]:
  def shape: Structured
  def json(e: E): String

object ToolError:
  given cannotFail: ToolError[Nothing] with
    val shape: Structured        = Structured.of(SchemaJson.render(heddle.endpoint.SchemaDoc.Null))
    def json(e: Nothing): String = e

  given typed[E](using s: Schema[E], j: JsonCodec[E]): ToolError[E] with
    val shape: Structured  = Structured.of(SchemaJson.render(s.doc))
    def json(e: E): String = j.encoder.encodeJson(e).toString
end ToolError
