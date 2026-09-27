package heddle.internal

import java.util.UUID
import zio.{Chunk, UIO, ZIO}

/** Identifiers and tokens from the platform CSPRNG (`SecureRandom`, Node `crypto`, OpenSSL `RAND_bytes`). */
private[heddle] object Ids:
  /** A CSPRNG that cannot produce bytes is a broken host, and a token from anything weaker is a vulnerability, so that
    * is a defect.
    */
  def bytes(n: Int): UIO[Chunk[Byte]] =
    ZIO.suspendSucceed {
      val out = new Array[Byte](n)
      if IdsPlatform.fill(out) then ZIO.succeed(Chunk.fromArray(out))
      else ZIO.dieMessage("the platform CSPRNG produced no bytes")
    }

  /** UUID v4 (RFC 9562 §5.4). */
  val uuid: UIO[UUID] =
    bytes(16).map { raw =>
      val b = raw.toArray
      b(6) = ((b(6) & 0x0f) | 0x40).toByte
      b(8) = ((b(8) & 0x3f) | 0x80).toByte
      val hi = b.take(8).foldLeft(0L)((acc, x) => (acc << 8) | (x & 0xff))
      val lo = b.drop(8).foldLeft(0L)((acc, x) => (acc << 8) | (x & 0xff))
      UUID(hi, lo)
    }

  /** 128 random bits as 32 lowercase hex characters: auth codes, refresh tokens, session ids. */
  val token: UIO[String] =
    bytes(16).map(_.map(b => f"${b & 0xff}%02x").mkString)
end Ids
