package heddle.http.header

import heddle.http.{ContentEncoding, MediaType, TransferCoding, Url}
import java.time.Instant
import zio.Chunk

final case class Headers(toChunk: Chunk[Header]):
  def get(name: HeaderName): Option[String] =
    toChunk.find(h => (h.name eq name) || h.name == name).map(_.value)

  def get(name: String): Option[String] = get(HeaderName(name))

  def getAll(name: HeaderName): Chunk[String] =
    toChunk.filter(h => (h.name eq name) || h.name == name).map(_.value)

  def getAll(name: String): Chunk[String] = getAll(HeaderName(name))

  def has(name: HeaderName): Boolean = get(name).isDefined

  def has(name: String): Boolean = has(HeaderName(name))

  def add(name: HeaderName, value: String): Headers =
    Headers(toChunk :+ Header(name, value))

  def add(name: String, value: String): Headers =
    add(HeaderName(name), value)

  def add(header: Header): Headers = Headers(toChunk :+ header)

  def ++(that: Headers): Headers = Headers(toChunk ++ that.toChunk)

  def remove(name: HeaderName): Headers =
    Headers(toChunk.filterNot(h => (h.name eq name) || h.name == name))

  def remove(name: String): Headers = remove(HeaderName(name))

  def withHeader(name: HeaderName, value: String): Headers =
    remove(name).add(name, value)

  def withHeader(name: String, value: String): Headers =
    withHeader(HeaderName(name), value)

  def isEmpty: Boolean = toChunk.isEmpty

  def get[A](using t: TypedHeader[A]): Option[A] =
    get(t.name).flatMap(v => t.decode(v).toOption)

  def getAll[A](using t: TypedHeader[A]): Chunk[A] =
    getAll(t.name).flatMap(v => Chunk.fromIterable(t.decode(v).toOption))

  def set[A](a: A)(using t: TypedHeader[A]): Headers =
    withHeader(t.name, t.encode(a))

  def contentType: Option[MediaType] = get[MediaType]

  def contentLength: Option[Long] =
    get(HeaderName.ContentLength).flatMap(_.toLongOption)

  def contentEncoding: Chunk[ContentEncoding] =
    ContentEncoding.parseList(get(HeaderName.ContentEncoding))

  def acceptEncoding: Chunk[ContentEncoding] =
    ContentEncoding.parseAccept(get(HeaderName.AcceptEncoding))

  def transferEncoding: Chunk[TransferCoding] =
    TransferCoding.parseList(get(HeaderName.TransferEncoding))

  def cookies: Chunk[CookiePair] =
    getAll(HeaderName.Cookie).flatMap(CookiePair.parseHeader)

  def setCookies: Chunk[SetCookie] =
    getAll(HeaderName.SetCookie).flatMap(raw => Chunk.fromIterable(SetCookie.parse(raw)))

  def location: Option[Url] =
    get(HeaderName.Location).flatMap { raw =>
      try Some(Url.parse(raw))
      catch case _: java.net.URISyntaxException => None
    }

  def date: Option[Instant]            = get(HeaderName.Date).flatMap(HttpDate.parse)
  def expires: Option[Instant]         = get(HeaderName.Expires).flatMap(HttpDate.parse)
  def lastModified: Option[Instant]    = get(HeaderName.LastModified).flatMap(HttpDate.parse)
  def ifModifiedSince: Option[Instant] = get(HeaderName.IfModifiedSince).flatMap(HttpDate.parse)

  def range: Option[RangeSpec] =
    get(HeaderName.Range).flatMap(RangeSpec.parse)

  def etag: Option[EntityTag] =
    get(HeaderName.ETag).flatMap(EntityTag.parse)

  def ifNoneMatch: Chunk[EntityTag] =
    get(HeaderName.IfNoneMatch).map(EntityTag.parseList).getOrElse(Chunk.empty)

  def ifMatch: Chunk[EntityTag] =
    get(HeaderName.IfMatch).map(EntityTag.parseList).getOrElse(Chunk.empty)
end Headers

object Headers:
  val empty: Headers = Headers(Chunk.empty)

  def apply(name: HeaderName, value: String): Headers =
    Headers(Chunk(Header(name, value)))

  def apply(name: String, value: String): Headers =
    apply(HeaderName(name), value)

  def apply(pairs: (HeaderName, String)*): Headers =
    Headers(Chunk.fromIterable(pairs.map((n, v) => Header(n, v))))

  def contentType(value: String): Headers = Headers(HeaderName.ContentType, value)

  def contentLength(n: Long): Headers = Headers(HeaderName.ContentLength, n.toString)
end Headers
