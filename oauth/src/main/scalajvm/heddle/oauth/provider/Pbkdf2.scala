package heddle.oauth.provider

import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import scala.util.Try
import zio.Chunk

/** The JCA's PBKDF2; `None` only on a JVM without it (every JDK since 8 has it). */
private[provider] object Pbkdf2:
  def sha256(password: String, salt: Chunk[Byte], iterations: Int): Option[Chunk[Byte]] =
    Try {
      val spec = PBEKeySpec(password.toCharArray, salt.toArray, iterations, 256)
      Chunk.fromArray(SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256").generateSecret(spec).getEncoded)
    }.toOption
