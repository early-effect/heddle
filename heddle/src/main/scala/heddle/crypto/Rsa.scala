package heddle.crypto

import heddle.error.HeddleError
import zio.{Chunk, IO, ULayer, ZIO}

final case class RsaPublic(n: Chunk[Byte], e: Chunk[Byte])

final case class RsaKey(public: RsaPublic, d: Chunk[Byte])

enum RsaOperation(val label: String):
  case Generate extends RsaOperation("key generation")
  case Sign     extends RsaOperation("signing")
  case Verify   extends RsaOperation("verification")

/** The platform's RSA refused an operation: a key it cannot use, or a provider that is missing. */
enum RsaError(val message: String) extends HeddleError:
  case Failed(operation: RsaOperation, reason: String) extends RsaError(s"RSA ${operation.label} failed: $reason")

/** RS256 on the platform's own RSA (JCA, Node `crypto`, OpenSSL). */
trait Rsa:
  def generate(bits: Int): IO[RsaError, RsaKey]
  def signSha256(key: RsaKey, payload: Chunk[Byte]): IO[RsaError, Chunk[Byte]]

  /** `false` for a signature that does not match; a failure only when the key itself is unusable. */
  def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): IO[RsaError, Boolean]

object Rsa:
  def generate(bits: Int): ZIO[Rsa, RsaError, RsaKey] =
    ZIO.serviceWithZIO(_.generate(bits))

  def signSha256(key: RsaKey, payload: Chunk[Byte]): ZIO[Rsa, RsaError, Chunk[Byte]] =
    ZIO.serviceWithZIO(_.signSha256(key, payload))

  def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): ZIO[Rsa, RsaError, Boolean] =
    ZIO.serviceWithZIO(_.verifySha256(pub, payload, sig))

  def live: ULayer[Rsa] = RsaPlatform.live
end Rsa
