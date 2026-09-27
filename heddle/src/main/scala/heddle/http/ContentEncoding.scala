package heddle.http

import zio.Chunk

/** A content coding (RFC 9110 §8.4.1). `Other` keeps a token heddle does not implement, so it is never mistaken for one
  * it does.
  */
enum ContentEncoding:
  case Gzip, Brotli, Identity, Deflate
  case Other(name: String)

  def token: String = this match
    case Gzip        => "gzip"
    case Brotli      => "br"
    case Identity    => "identity"
    case Deflate     => "deflate"
    case Other(name) => name
end ContentEncoding

/** Which codings an `Accept-Encoding` member covers: one coding, or `*` for every coding it does not name. */
enum EncodingRange:
  case Any
  case Only(coding: ContentEncoding)

/** One `Accept-Encoding` member and its quality, 0 to 1 (RFC 9110 §12.5.3). Quality 0 means "not acceptable". */
final case class AcceptEncoding(range: EncodingRange, quality: Double)

object ContentEncoding:
  def fromToken(s: String): ContentEncoding =
    HeaderParams.lower(s.trim) match
      case "gzip" | "x-gzip" => Gzip
      case "br"              => Brotli
      case "identity"        => Identity
      case "deflate"         => Deflate
      case other             => Other(other)

  /** The codings a `Content-Encoding` names, in the order they were applied. */
  def parseList(header: Option[String]): Chunk[ContentEncoding] =
    Chunk.fromIterable(header.toList.flatMap(members).map(m => fromToken(m.takeWhile(_ != ';'))))

  /** Every member with a valid quality; a member whose `q` is not a number from 0 to 1 is ignored. */
  def parseAccept(header: String): Chunk[AcceptEncoding] =
    Chunk.fromIterable(members(header).flatMap { member =>
      val token   = member.takeWhile(_ != ';').trim
      val semi    = member.indexOf(';')
      val params  = if semi < 0 then Nil else HeaderParams.parse(member, semi + 1)
      val quality =
        params.collectFirst { case ("q", v) => v }.fold(Option(1.0))(_.toDoubleOption.filter(q => q >= 0 && q <= 1))
      val range = if token == "*" then EncodingRange.Any else EncodingRange.Only(fromToken(token))
      quality.map(AcceptEncoding(range, _))
    })

  /** The coding in `available` the client rates highest, ties going to the earlier one; `None` for identity. A coding
    * named with quality 0 is refused even when `*` would cover it, and a client that sends no `Accept-Encoding` gets
    * identity.
    */
  def negotiate(accept: Option[String], available: Chunk[ContentEncoding]): Option[ContentEncoding] =
    accept.flatMap { raw =>
      val members                             = parseAccept(raw)
      val any                                 = members.collectFirst { case AcceptEncoding(EncodingRange.Any, q) => q }
      def quality(c: ContentEncoding): Double =
        members.collectFirst { case AcceptEncoding(EncodingRange.Only(`c`), q) => q }.orElse(any).getOrElse(0.0)
      available.zipWithIndex
        .filter((c, _) => c != Identity && quality(c) > 0)
        .sortBy((c, i) => (-quality(c), i))
        .headOption
        .map(_._1)
    }

  private def members(raw: String): List[String] =
    raw.split(',').toList.map(_.trim).filter(_.nonEmpty)
end ContentEncoding
