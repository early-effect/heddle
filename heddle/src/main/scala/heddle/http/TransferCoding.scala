package heddle.http

import heddle.endpoint.SchemaDoc
import heddle.error.ParamError
import heddle.http.header.{HeaderName, TypedHeader}
import zio.Chunk

/** A transfer coding (RFC 9112 §7). `Other` keeps a token heddle does not implement. */
enum TransferCoding:
  case Chunked, Compress, Deflate, Gzip
  case Other(name: String)

  def token: String = this match
    case Chunked     => "chunked"
    case Compress    => "compress"
    case Deflate     => "deflate"
    case Gzip        => "gzip"
    case Other(name) => name
end TransferCoding

object TransferCoding:
  def fromToken(s: String): TransferCoding =
    HeaderParams.lower(s.trim) match
      case "chunked"  => Chunked
      case "compress" => Compress
      case "deflate"  => Deflate
      case "gzip"     => Gzip
      case other      => Other(other)

  def parseList(header: Option[String]): Chunk[TransferCoding] =
    Chunk.fromIterable(
      header.toList
        .flatMap(_.split(',').toList)
        .map(_.takeWhile(_ != ';').trim)
        .filter(_.nonEmpty)
        .map(fromToken)
    )

  /** Every coding named by `Transfer-Encoding`, one wire line per coding. */
  given transferEncodingHeader: TypedHeader[Chunk[TransferCoding]] =
    TypedHeader.several(HeaderName.TransferEncoding, SchemaDoc.Array(SchemaDoc.Str(None)))(
      values =>
        if values.isEmpty then Left(ParamError.Missing)
        else Right(values.flatMap(line => parseList(Some(line)))),
      codings => codings.map(_.token),
    )
end TransferCoding
