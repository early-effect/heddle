package heddle.crypto

import zio.{Chunk, UIO, ULayer, URIO, ZIO}

trait Digest:
  def sha256(bytes: Chunk[Byte]): UIO[Chunk[Byte]]

object Digest:
  def sha256(bytes: Chunk[Byte]): URIO[Digest, Chunk[Byte]] =
    ZIO.serviceWithZIO(_.sha256(bytes))

  def live: ULayer[Digest] = DigestPlatform.live
