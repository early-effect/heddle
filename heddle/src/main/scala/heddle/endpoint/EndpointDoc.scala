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
)
