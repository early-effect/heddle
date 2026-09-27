package heddle.mcp

import heddle.endpoint.{EndpointDoc, SchemaDoc, SchemaJson}
import heddle.mcp.protocol.Structured

/** How an operation's typed output and typed error travel as `structuredContent`. The server encodes with these and the
  * client decodes with them, so both read the shape from the same `EndpointDoc`.
  */
object ToolShapes:
  def output(doc: EndpointDoc): Option[Structured] =
    doc.responses.find(r => r.status.code >= 200 && r.status.code < 300).flatMap(_.schema).map { s =>
      Structured.of(SchemaJson.render(s))
    }

  /** The error statuses' schemas: one as is, several as a `oneOf`. */
  def error(doc: EndpointDoc): Structured =
    val schemas = doc.responses.filterNot(_.status.isSuccess).flatMap(_.schema).distinct
    val schema  = schemas match
      case one :: Nil => one
      case many       => SchemaDoc.OneOf(None, many)
    Structured.of(SchemaJson.render(schema))
end ToolShapes
