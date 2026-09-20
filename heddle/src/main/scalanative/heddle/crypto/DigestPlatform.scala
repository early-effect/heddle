package heddle.crypto

import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object DigestPlatform:
  def live: ULayer[Digest] = ZLayer.succeed(Unsupported)

  private object Unsupported extends Digest:
    def sha256(bytes: Chunk[Byte]): UIO[Chunk[Byte]] =
      ZIO.dieMessage(s"Digest.sha256(${bytes.length}) is not implemented on this platform yet")
end DigestPlatform
