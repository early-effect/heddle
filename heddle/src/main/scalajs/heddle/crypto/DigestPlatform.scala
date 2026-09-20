package heddle.crypto

import heddle.internal.node.{Buffers, Crypto}
import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object DigestPlatform:
  def sha256Sync(bytes: Chunk[Byte]): Chunk[Byte] =
    Buffers.fromU8(Crypto.createHash("sha256").update(Buffers.toU8(bytes)).digest())

  def live: ULayer[Digest] = ZLayer.succeed(Live)

  private object Live extends Digest:
    def sha256(bytes: Chunk[Byte]): UIO[Chunk[Byte]] = ZIO.succeed(sha256Sync(bytes))
end DigestPlatform
