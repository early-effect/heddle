package heddle.mcp.auth

import heddle.http.{Method, Response}
import heddle.http.header.{AuthScheme, WwwAuthenticate}
import heddle.route.Routes
import heddle.route.PathDsl.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.Chunk

object ProtectedResource:
  def routes(
      resource: String,
      authorizationServers: List[String],
      scopes: List[String] = Nil,
  ): Routes[Any, Nothing] =
    val doc  = document(resource, authorizationServers, scopes)
    val body = Response.json(doc)
    Routes(
      (Method.GET / ".well-known" / "oauth-protected-resource")         -> body,
      (Method.GET / ".well-known" / "oauth-protected-resource" / "mcp") -> body,
    )
  end routes

  def unauthorized(resourceMetadata: String, scopes: List[String] = Nil): Response =
    val params =
      List("realm" -> "mcp", "resource_metadata" -> resourceMetadata) ++
        (if scopes.isEmpty then Nil else List("scope" -> scopes.mkString(" ")))
    Response.unauthorized("Unauthorized", WwwAuthenticate(AuthScheme.Bearer, params))

  def document(resource: String, authorizationServers: List[String], scopes: List[String]): String =
    val fields =
      List(
        "resource"                 -> Json.Str(resource),
        "authorization_servers"    -> Json.Arr(Chunk.fromIterable(authorizationServers.map(Json.Str(_)))),
        "bearer_methods_supported" -> Json.Arr(Json.Str("header")),
      ) ++ (if scopes.isEmpty then Nil
            else List("scopes_supported" -> Json.Arr(Chunk.fromIterable(scopes.map(Json.Str(_))))))
    Json.Obj(Chunk.fromIterable(fields)).toJson
end ProtectedResource
