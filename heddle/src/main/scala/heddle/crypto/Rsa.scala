package heddle.crypto

import zio.{Chunk, UIO, ULayer, URIO, ZIO}

final case class RsaPublic(n: Chunk[Byte], e: Chunk[Byte])

final case class RsaKey(public: RsaPublic, d: Chunk[Byte])

trait Rsa:
  def generate(bits: Int): UIO[RsaKey]
  def signSha256(key: RsaKey, payload: Chunk[Byte]): UIO[Chunk[Byte]]
  def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): UIO[Boolean]

object Rsa:
  def generate(bits: Int): URIO[Rsa, RsaKey] =
    ZIO.serviceWithZIO(_.generate(bits))

  def signSha256(key: RsaKey, payload: Chunk[Byte]): URIO[Rsa, Chunk[Byte]] =
    ZIO.serviceWithZIO(_.signSha256(key, payload))

  def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): URIO[Rsa, Boolean] =
    ZIO.serviceWithZIO(_.verifySha256(pub, payload, sig))

  def live: ULayer[Rsa] = RsaPlatform.live
end Rsa
