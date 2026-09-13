package heddle.http

import zio.Chunk

enum ContentEncoding:
  case Gzip, Brotli, Identity, Deflate, Other

  def token: String = this match
    case Gzip     => "gzip"
    case Brotli   => "br"
    case Identity => "identity"
    case Deflate  => "deflate"
    case Other    => "*"

object ContentEncoding:
  def fromToken(s: String): ContentEncoding =
    s.trim.toLowerCase match
      case "gzip"     => Gzip
      case "br"       => Brotli
      case "identity" => Identity
      case "deflate"  => Deflate
      case _          => Other

  /** Client preference order, q-values ignored. Missing header is identity. */
  def parseAccept(header: Option[String]): Chunk[ContentEncoding] =
    val listed = parseList(header)
    if listed.isEmpty then Chunk(Identity) else listed

  def parseList(header: Option[String]): Chunk[ContentEncoding] =
    header match
      case None | Some("") => Chunk.empty
      case Some(raw)       =>
        val parts = raw.split(',').toList.map(_.split(';')(0).trim).filter(_.nonEmpty)
        Chunk.fromIterable(parts.map(fromToken))
end ContentEncoding
