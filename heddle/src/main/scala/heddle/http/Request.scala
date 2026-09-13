package heddle.http

import heddle.http.header.{CookiePair, Header, HeaderName, Headers}

final case class Request(
    method: Method,
    url: Url,
    headers: Headers = Headers.empty,
    body: Body = Body.empty,
    version: HttpVersion = HttpVersion.Http11,
    secure: Boolean = false,
):
  def path: Path                               = url.path
  def query: QueryParams                       = url.query
  def header(name: HeaderName): Option[String] = headers.get(name)
  def header(name: String): Option[String]     = headers.get(name)

  def addHeader(name: HeaderName, value: String): Request =
    copy(headers = headers.add(name, value))

  def addHeader(name: String, value: String): Request =
    addHeader(HeaderName(name), value)

  def addHeader(header: Header): Request =
    copy(headers = headers.add(header))

  def withHeader(name: HeaderName, value: String): Request =
    copy(headers = headers.withHeader(name, value))

  def withHeader(name: String, value: String): Request =
    withHeader(HeaderName(name), value)

  def cookies: zio.Chunk[CookiePair] = headers.cookies

  def cookie(name: String): Option[String] =
    cookies.find(_.name == name).map(_.value)

  def addCookie(name: String, value: String): Request =
    val existing = cookies
    val next     = (existing.filterNot(_.name == name) :+ CookiePair(name, value))
      .map(c => s"${c.name}=${c.value}")
      .mkString("; ")
    copy(headers = headers.withHeader(HeaderName.Cookie, next))
end Request

object Request:
  def get(path: String): Request =
    Request(Method.GET, Url.parse(path))

  def get(url: Url): Request =
    Request(Method.GET, url)

  def post(path: String, body: Body): Request =
    Request(Method.POST, Url.parse(path), body = body)
