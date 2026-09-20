package heddle.crypto

import java.util.Base64
import zio.Chunk

private[heddle] object Base64Url:
  def encode(bytes: Chunk[Byte]): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes.toArray)

  def decode(raw: String): Either[String, Chunk[Byte]] =
    try Right(Chunk.fromArray(Base64.getUrlDecoder.decode(raw)))
    catch case e: IllegalArgumentException => Left(Option(e.getMessage).getOrElse("invalid base64url"))
end Base64Url
