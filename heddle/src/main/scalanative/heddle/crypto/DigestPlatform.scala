package heddle.crypto

import heddle.internal.openssl.Ssl
import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object DigestPlatform:
  def sha256Sync(bytes: Chunk[Byte]): Chunk[Byte] =
    Chunk.fromArray(Ssl.sha256(bytes.toArray))

  def live: ULayer[Digest] = ZLayer.succeed(Live)

  private object Live extends Digest:
    def sha256(bytes: Chunk[Byte]): UIO[Chunk[Byte]] = ZIO.succeed(sha256Sync(bytes))
end DigestPlatform
