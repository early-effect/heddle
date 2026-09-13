package heddle.auth

import heddle.endpoint.ApiKeyIn
import heddle.http.{Request, Response}
import heddle.http.header.{AuthScheme, Authorization, BasicCredentials, WwwAuthenticate}
import heddle.http.header.Authorization.given
import zio.{IO, ZIO}

object Auth:
  val unauthorizedBasic: Response =
    Response.unauthorized(challenge = WwwAuthenticate(AuthScheme.Basic, List("realm" -> "api")))

  val unauthorizedBearer: Response =
    Response.unauthorized(challenge = WwwAuthenticate(AuthScheme.Bearer, List("realm" -> "api")))

  def unauthorizedApiKey(name: String): Response =
    Response.unauthorized(s"missing $name", WwwAuthenticate(AuthScheme.Other("ApiKey"), List("realm" -> "api")))

  def basic(validate: BasicCredentials => Boolean): Request => IO[Response, BasicCredentials] =
    basicZIO(c => if validate(c) then ZIO.succeed(c) else ZIO.fail(unauthorizedBasic))

  def basicZIO[R](
      validate: BasicCredentials => ZIO[R, Response, BasicCredentials]
  ): Request => ZIO[R, Response, BasicCredentials] =
    req =>
      req.headers.get[Authorization].flatMap(BasicCredentials.parse) match
        case None    => ZIO.fail(unauthorizedBasic)
        case Some(c) => validate(c)

  def bearer[R, P](validate: String => ZIO[R, Response, P]): Request => ZIO[R, Response, P] =
    req =>
      req.headers.get[Authorization] match
        case Some(Authorization(AuthScheme.Bearer, token)) if token.nonEmpty => validate(token)
        case _                                                               => ZIO.fail(unauthorizedBearer)

  def apiKey[R, P](
      in: ApiKeyIn,
      name: String,
      validate: String => ZIO[R, Response, P],
  ): Request => ZIO[R, Response, P] =
    req =>
      val raw = in match
        case ApiKeyIn.Header => req.header(name)
        case ApiKeyIn.Query  => req.query.get(name)
        case ApiKeyIn.Cookie => req.cookie(name)
      raw match
        case Some(v) => validate(v)
        case None    => ZIO.fail(unauthorizedApiKey(name))
end Auth
