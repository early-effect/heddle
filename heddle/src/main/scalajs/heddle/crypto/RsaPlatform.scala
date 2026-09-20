package heddle.crypto

import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object RsaPlatform:
  def live: ULayer[Rsa] = ZLayer.succeed(Unsupported)

  private object Unsupported extends Rsa:
    def generate(bits: Int): UIO[RsaKey] =
      ZIO.dieMessage(s"Rsa.generate($bits) is not implemented on this platform yet")

    def signSha256(key: RsaKey, payload: Chunk[Byte]): UIO[Chunk[Byte]] =
      ZIO.dieMessage(
        s"Rsa.signSha256(${key.public.n.length},${payload.length}) is not implemented on this platform yet"
      )

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): UIO[Boolean] =
      ZIO.dieMessage(
        s"Rsa.verifySha256(${pub.n.length},${payload.length},${sig.length}) is not implemented on this platform yet"
      )
  end Unsupported
end RsaPlatform
