package heddle.endpoint

enum ApiKeyIn:
  case Header, Query, Cookie

final case class OAuthFlow(
    authorizationUrl: Option[String] = None,
    tokenUrl: Option[String] = None,
    refreshUrl: Option[String] = None,
    scopes: Map[String, String] = Map.empty,
)

final case class OAuthFlows(
    authorizationCode: Option[OAuthFlow] = None,
    clientCredentials: Option[OAuthFlow] = None,
    password: Option[OAuthFlow] = None,
)

enum SecurityScheme:
  case HttpBasic(schemeName: String = "basic")
  case HttpBearer(schemeName: String = "bearer", bearerFormat: Option[String] = None)
  case ApiKey(schemeName: String, in: ApiKeyIn, paramName: String)
  case OAuth2(schemeName: String, flows: OAuthFlows)
  case OpenIdConnect(schemeName: String, issuerUrl: String)

  def name: String =
    this match
      case HttpBasic(n)        => n
      case HttpBearer(n, _)    => n
      case ApiKey(n, _, _)     => n
      case OAuth2(n, _)        => n
      case OpenIdConnect(n, _) => n
end SecurityScheme
