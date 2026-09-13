package heddle.http

import heddle.http.header.{HeaderName, Headers, SetCookie, WwwAuthenticate}
import heddle.http.header.WwwAuthenticate.given

final class Response private[heddle] (
    val status: Status,
    val headers: Headers,
    val body: Body,
    private[heddle] val ws: Option[heddle.ws.WsUpgrade],
):
  def header(name: HeaderName): Option[String] = headers.get(name)
  def header(name: String): Option[String]     = headers.get(name)

  def addHeader(name: HeaderName, value: String): Response =
    copy(headers = headers.add(name, value))

  def addHeader(name: String, value: String): Response =
    addHeader(HeaderName(name), value)

  def withHeader(name: HeaderName, value: String): Response =
    copy(headers = headers.withHeader(name, value))

  def withHeader(name: String, value: String): Response =
    withHeader(HeaderName(name), value)

  def addCookie(cookie: SetCookie): Response =
    addHeader(HeaderName.SetCookie, SetCookie.render(cookie))

  def clearCookie(name: String, path: Option[String] = Some("/")): Response =
    addCookie(SetCookie(name, "", path = path, maxAge = Some(0)))

  def withBody(body: Body): Response =
    val withType =
      body.mediaType.fold(headers)(mt =>
        if headers.has(HeaderName.ContentType) then headers
        else headers.add(HeaderName.ContentType, mt.render)
      )
    copy(headers = withType, body = body)

  def copy(
      status: Status = this.status,
      headers: Headers = this.headers,
      body: Body = this.body,
  ): Response =
    new Response(status, headers, body, ws = this.ws)

  override def equals(other: Any): Boolean =
    other match
      case r: Response => status == r.status && headers == r.headers && body == r.body
      case _           => false

  override def hashCode: Int = (status, headers, body).hashCode

  override def toString: String = s"Response($status,$headers,$body)"
end Response

object Response:
  def apply(
      status: Status = Status.Ok,
      headers: Headers = Headers.empty,
      body: Body = Body.empty,
  ): Response =
    new Response(status, headers, body, ws = None)

  def apply(status: Status, body: Body): Response =
    apply(status, Headers.empty, body).withBody(body)

  private[heddle] def withWs(status: Status, headers: Headers, run: heddle.ws.WsUpgrade): Response =
    new Response(status, headers, Body.empty, ws = Some(run))

  def fromServerSentEvents(events: zio.stream.ZStream[Any, Throwable, heddle.sse.ServerSentEvent]): Response =
    heddle.sse.Sse.response(events)

  def text(value: String, status: Status = Status.Ok): Response =
    Response(status).withBody(Body.text(value))

  def html(value: String, status: Status = Status.Ok): Response =
    Response(status).withBody(Body.html(value))

  def json(value: String, status: Status = Status.Ok): Response =
    Response(status).withBody(Body.json(value))

  def empty(status: Status = Status.NoContent): Response =
    Response(status)

  val ok: Response = empty(Status.Ok)

  def notFound(message: String = "Not Found"): Response =
    text(message, Status.NotFound)

  def badRequest(message: String): Response =
    text(message, Status.BadRequest)

  def unauthorized(
      message: String = "Unauthorized",
      challenge: WwwAuthenticate = WwwAuthenticate(heddle.http.header.AuthScheme.Bearer, List("realm" -> "api")),
  ): Response =
    val res = text(message, Status.Unauthorized)
    res.copy(headers = res.headers.set(challenge))

  def redirect(location: String, status: Status = Status.Found): Response =
    empty(status).withHeader(HeaderName.Location, location)

  def methodNotAllowed(allow: String): Response =
    text("Method Not Allowed", Status.MethodNotAllowed).withHeader("Allow", allow)

  def internalServerError(message: String = "Internal Server Error"): Response =
    text(message, Status.InternalServerError)
end Response
