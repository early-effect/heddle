package heddle.http

import zio.Chunk

enum TransferCoding:
  case Chunked, Compress, Deflate, Gzip, Other

  def token: String = this match
    case Chunked  => "chunked"
    case Compress => "compress"
    case Deflate  => "deflate"
    case Gzip     => "gzip"
    case Other    => "*"

object TransferCoding:
  def fromToken(s: String): TransferCoding =
    s.trim.toLowerCase match
      case "chunked"  => Chunked
      case "compress" => Compress
      case "deflate"  => Deflate
      case "gzip"     => Gzip
      case _          => Other

  def parseList(header: Option[String]): Chunk[TransferCoding] =
    header match
      case None | Some("") => Chunk.empty
      case Some(raw)       =>
        val parts = raw.split(',').toList.map(_.split(';')(0).trim).filter(_.nonEmpty)
        Chunk.fromIterable(parts.map(fromToken))
end TransferCoding
