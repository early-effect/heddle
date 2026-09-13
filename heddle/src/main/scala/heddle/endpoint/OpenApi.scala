package heddle.endpoint

import heddle.internal.openapi.OpenApiJson
import heddle.route.Routes

final case class OpenApi(
    title: String,
    version: String,
    endpoints: List[EndpointDoc],
    description: Option[String] = None,
):
  def toJson: String = OpenApiJson.render(this)

  def routes(prefix: String = "docs"): Routes[Any, Nothing] =
    SwaggerUI.routes(this, prefix)
end OpenApi

object OpenApi:
  def from(title: String, version: String, endpoints: Endpoint[?, ?, ?]*): OpenApi =
    OpenApi(title, version, endpoints.map(_.doc).toList)
