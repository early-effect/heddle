package heddle.oauth.provider

import heddle.internal.node.{Buffers, Crypto}
import scala.util.Try
import zio.Chunk

/** Node's `crypto.pbkdf2Sync`. */
private[provider] object Pbkdf2:
  def sha256(password: String, salt: Chunk[Byte], iterations: Int): Option[Chunk[Byte]] =
    Try(Buffers.fromU8(Crypto.pbkdf2Sync(password, Buffers.toU8(salt), iterations, 32, "sha256"))).toOption
