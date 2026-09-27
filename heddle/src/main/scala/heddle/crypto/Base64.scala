package heddle.crypto

import zio.Chunk

/** RFC 4648 §4 base64 with its padding, as HTTP Basic credentials carry it (RFC 7617 §2). */
private[heddle] object Base64:
  /** Checked before the JDK sees it, so the decoder never throws: the alphabet, whole quanta, and padding only at the
    * end.
    */
  def decode(raw: String): Either[Base64Error, Chunk[Byte]] =
    val body    = raw.stripSuffix("==").stripSuffix("=")
    val padding = raw.length - body.length
    val whole   = (body.length % 4, padding) match
      case (0, 0) | (2, 2) | (3, 1) => true
      case _                        => false
    body.indexWhere(c => !(c.isLetterOrDigit && c < 128 || c == '+' || c == '/')) match
      case -1 if !whole => Left(Base64Error.BadLength(raw.length))
      case -1           => Right(Chunk.fromArray(java.util.Base64.getDecoder.decode(raw)))
      case at           => Left(Base64Error.BadCharacter(body(at), at))
  end decode

  def encode(bytes: Chunk[Byte]): String =
    java.util.Base64.getEncoder.encodeToString(bytes.toArray)
end Base64
