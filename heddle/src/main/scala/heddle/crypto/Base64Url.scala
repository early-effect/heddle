package heddle.crypto

import java.util.Base64
import zio.Chunk

private[heddle] object Base64Url:
  def encode(bytes: Chunk[Byte]): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes.toArray)

  /** Checked before the JDK sees it, so the decoder never throws: the alphabet, then a length that encodes whole bytes.
    */
  def decode(raw: String): Either[Base64Error, Chunk[Byte]] =
    val body = raw.stripSuffix("==").stripSuffix("=")
    body.indexWhere(c => !(c.isLetterOrDigit && c < 128 || c == '-' || c == '_')) match
      case -1 if body.length % 4 == 1 => Left(Base64Error.BadLength(body.length))
      case -1 => Right(Chunk.fromArray(Base64.getUrlDecoder.decode(body)))
      case at => Left(Base64Error.BadCharacter(body(at), at))
end Base64Url
