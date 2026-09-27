package heddle.oauth.provider

import heddle.crypto.Base64Url
import heddle.internal.Ids
import zio.{Chunk, UIO, ZIO}

/** Salted PBKDF2-HMAC-SHA256 (NIST SP 800-132), stored as `pbkdf2-sha256$<iterations>$<salt>$<hash>` in base64url. */
object Passwords:
  /** OWASP's 2023 floor for PBKDF2-HMAC-SHA256. */
  val DefaultIterations: Int = 600_000

  private val Prefix = "pbkdf2-sha256"

  def hash(plain: String, iterations: Int = DefaultIterations): UIO[String] =
    Ids.bytes(16).flatMap { salt =>
      ZIO.blocking(ZIO.succeed(Pbkdf2.sha256(plain, salt, iterations))).flatMap {
        case Some(key) => ZIO.succeed(s"$Prefix$$$iterations$$${Base64Url.encode(salt)}$$${Base64Url.encode(key)}")
        case None      => ZIO.dieMessage("PBKDF2-HMAC-SHA256 is unavailable on this platform")
      }
    }

  /** Constant-time in the stored hash. A malformed or legacy hash never matches. */
  def check(plain: String, stored: String): Boolean =
    stored.split('$') match
      case Array(Prefix, rounds, salt, key) =>
        val parts =
          for
            n <- rounds.toIntOption.filter(_ > 0)
            s <- Base64Url.decode(salt).toOption
            k <- Base64Url.decode(key).toOption
            d <- Pbkdf2.sha256(plain, s, n)
          yield sameBytes(d, k)
        parts.contains(true)
      case _ => false

  /** Checks `plain` against the record's hash, or against a decoy when there is no record, so an unknown username costs
    * the same time as a wrong password.
    */
  def authenticate[A](plain: String, record: Option[A])(hashOf: A => String): UIO[Option[A]] =
    ZIO.blocking(ZIO.succeed {
      val ok = check(plain, record.fold(Decoy)(hashOf))
      record.filter(_ => ok)
    })

  private val Decoy =
    s"$Prefix$$$DefaultIterations$$AAAAAAAAAAAAAAAAAAAAAA$$AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

  private def sameBytes(a: Chunk[Byte], b: Chunk[Byte]): Boolean =
    a.length == b.length && a.zip(b).foldLeft(0)((acc, xy) => acc | (xy._1 ^ xy._2)) == 0
end Passwords
