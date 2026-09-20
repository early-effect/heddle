package heddle.crypto

import heddle.internal.openssl.Ssl
import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object RsaPlatform:
  def generateSync(bits: Int): RsaKey =
    val (n, e, d) = Ssl.rsaGenerate(bits)
    RsaKey(RsaPublic(Chunk.fromArray(n), Chunk.fromArray(e)), Chunk.fromArray(d))

  def signSync(key: RsaKey, payload: Chunk[Byte]): Chunk[Byte] =
    Chunk.fromArray(Ssl.rsaSign(key.public.n.toArray, key.public.e.toArray, key.d.toArray, payload.toArray))

  def verifySync(pub: RsaPublic, payload: Chunk[Byte], sigBytes: Chunk[Byte]): Boolean =
    Ssl.rsaVerify(pub.n.toArray, pub.e.toArray, payload.toArray, sigBytes.toArray)

  def live: ULayer[Rsa] = ZLayer.succeed(Live)

  private object Live extends Rsa:
    def generate(bits: Int): UIO[RsaKey] =
      ZIO.succeed(generateSync(bits))

    def signSha256(key: RsaKey, payload: Chunk[Byte]): UIO[Chunk[Byte]] =
      ZIO.succeed(signSync(key, payload))

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): UIO[Boolean] =
      ZIO.succeed(verifySync(pub, payload, sig))
end RsaPlatform
