package heddle.endpoint

import heddle.http.{MediaType, Method, Status}

final case class ParamDoc(name: String, in: String, required: Boolean, schema: SchemaDoc)

final case class MediaDoc(schema: SchemaDoc, contentType: MediaType)

final case class StatusDoc(
    status: Status,
    schema: Option[SchemaDoc],
    contentType: Option[MediaType],
    description: String,
)

final case class EndpointDoc(
    method: Method,
    pathTemplate: String,
    pathParams: List[ParamDoc],
    queries: List[ParamDoc],
    headers: List[ParamDoc],
    requestBody: Option[MediaDoc],
    responses: List[StatusDoc],
    operationId: Option[String],
    summary: Option[String],
    description: Option[String],
    tags: List[String],
    security: List[SecurityScheme] = Nil,
    promoted: Boolean = false,
    mcpName: Option[String] = None,
    nestBody: Boolean = false,
    hints: List[Hint] = Nil,
):
  def toolName: String =
    mcpName.filter(_.nonEmpty).orElse(operationId).getOrElse(derivedToolName)

  private def derivedToolName: String =
    val segs = pathTemplate.split('/').toList.filter(_.nonEmpty).map(_.filterNot(c => c == '{' || c == '}'))
    (method.render.toLowerCase :: segs).filter(_.nonEmpty).mkString("_")
end EndpointDoc
