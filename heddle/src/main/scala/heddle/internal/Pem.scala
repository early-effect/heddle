package heddle.internal

import heddle.error.TlsError
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.util.Try

/** The DER bytes of the first PEM block of one kind (`CERTIFICATE`, `PRIVATE KEY`). */
private[heddle] object Pem:
  def body(pem: String, kind: String): Either[TlsError, Array[Byte]] =
    val begin = s"-----BEGIN $kind-----"
    val end   = s"-----END $kind-----"
    val from  = pem.indexOf(begin)
    val until = pem.indexOf(end)
    if from < 0 || until < from then Left(TlsError.MissingPem(kind))
    else
      val b64 = pem.substring(from + begin.length, until).filterNot(_.isWhitespace)
      Try(Base64.getDecoder.decode(b64.getBytes(StandardCharsets.US_ASCII))).toEither.left.map(TlsError.Unusable(_))
end Pem
