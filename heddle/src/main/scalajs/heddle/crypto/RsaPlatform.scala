package heddle.crypto

import heddle.internal.node.{Buffers, Crypto, GenerateKeyOptions, JwkExport, JwkKeyInput, NodeBuffer, SignKeyOptions}
import java.math.BigInteger
import scala.scalajs.js
import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** Node `crypto` RSA. Node reports failure by throwing, so each call is attempted here and becomes an `RsaError`. */
private[heddle] object RsaPlatform:
  def live: ULayer[Rsa] = ZLayer.succeed(Live)

  private object Live extends Rsa:
    def generate(bits: Int): IO[RsaError, RsaKey] =
      ZIO
        .attempt {
          val pair = Crypto.generateKeyPairSync("rsa", GenerateKeyOptions(bits))
          pair.privateKey.exportJwk(JwkExport("jwk"))
        }
        .mapError(failed(RsaOperation.Generate))
        .flatMap { jwk =>
          jwk.d.toOption match
            case Some(d) => ZIO.succeed(RsaKey(RsaPublic(b64(jwk.n), b64(jwk.e)), b64(d)))
            case None    => ZIO.fail(RsaError.Failed(RsaOperation.Generate, "Node exported a private key with no d"))
        }

    def signSha256(key: RsaKey, payload: Chunk[Byte]): IO[RsaError, Chunk[Byte]] =
      ZIO
        .fromEither(privateJwk(key))
        .flatMap { jwk =>
          ZIO
            .attempt {
              val ko   = Crypto.createPrivateKey(JwkKeyInput(jwk, "jwk"))
              val opts = SignKeyOptions(ko, padding = Crypto.constants.RSA_PKCS1_PADDING)
              Buffers.fromU8(Crypto.sign("sha256", Buffers.toU8(payload), opts))
            }
            .mapError(failed(RsaOperation.Sign))
        }

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): IO[RsaError, Boolean] =
      ZIO
        .attempt {
          val ko = Crypto.createPublicKey(JwkKeyInput(publicJwk(pub), "jwk"))
          SignKeyOptions(ko, padding = Crypto.constants.RSA_PKCS1_PADDING)
        }
        .mapError(failed(RsaOperation.Verify))
        .flatMap { opts =>
          ZIO
            .attempt(Crypto.verify("sha256", Buffers.toU8(payload), opts, Buffers.toU8(sig)))
            .mapError(failed(RsaOperation.Verify))
        }
  end Live

  private def failed(operation: RsaOperation)(cause: Throwable): RsaError =
    RsaError.Failed(operation, cause.toString)

  /** Node `createPrivateKey` rejects RSA JWKs that only have `n`/`e`/`d`. Recover CRT factors. */
  private def privateJwk(key: RsaKey): Either[RsaError, js.Dictionary[String]] =
    val n = integer(key.public.n)
    val e = integer(key.public.e)
    val d = integer(key.d)
    recoverPrimes(n, e, d).toRight(RsaError.Failed(RsaOperation.Sign, "the key's primes cannot be recovered")).map {
      (p, q) =>
        privateJwk(n, e, d, p, q)
    }

  private def privateJwk(
      n: BigInteger,
      e: BigInteger,
      d: BigInteger,
      p: BigInteger,
      q: BigInteger,
  ): js.Dictionary[String] =
    val dp = d.mod(p.subtract(BigInteger.ONE))
    val dq = d.mod(q.subtract(BigInteger.ONE))
    val qi = q.modInverse(p)
    js.Dictionary(
      "kty" -> "RSA",
      "n"   -> b64enc(unsigned(n)),
      "e"   -> b64enc(unsigned(e)),
      "d"   -> b64enc(unsigned(d)),
      "p"   -> b64enc(unsigned(p)),
      "q"   -> b64enc(unsigned(q)),
      "dp"  -> b64enc(unsigned(dp)),
      "dq"  -> b64enc(unsigned(dq)),
      "qi"  -> b64enc(unsigned(qi)),
    )
  end privateJwk

  private def publicJwk(pub: RsaPublic): js.Dictionary[String] =
    js.Dictionary(
      "kty" -> "RSA",
      "n"   -> b64enc(pub.n),
      "e"   -> b64enc(pub.e),
    )

  private def recoverPrimes(n: BigInteger, e: BigInteger, d: BigInteger): Option[(BigInteger, BigInteger)] =
    val one = BigInteger.ONE
    val nm1 = n.subtract(one)
    var t   = d.multiply(e).subtract(one)
    var s   = 0
    while !t.testBit(0) do
      t = t.shiftRight(1)
      s += 1
    var aInt  = 2
    var found = Option.empty[(BigInteger, BigInteger)]
    while found.isEmpty && aInt < 256 do
      var x    = BigInteger.valueOf(aInt.toLong).modPow(t, n)
      var i    = 0
      var stop = false
      while i < s && !stop && found.isEmpty do
        val y = x.multiply(x).mod(n)
        if y == one && x != one && x != nm1 then
          val p = x.subtract(one).gcd(n)
          if p.compareTo(one) > 0 && p.compareTo(n) < 0 then found = Some((p, n.divide(p)))
        if y == one || y == nm1 then stop = true
        else
          x = y
          i += 1
      aInt += 1
    end while
    found
  end recoverPrimes

  private def integer(bytes: Chunk[Byte]): BigInteger =
    BigInteger(1, bytes.toArray)

  private def unsigned(n: BigInteger): Chunk[Byte] =
    val raw = n.toByteArray
    if raw.length > 1 && raw(0) == 0 then Chunk.fromArray(raw.drop(1))
    else Chunk.fromArray(raw)

  private def b64(raw: String): Chunk[Byte] =
    Buffers.fromU8(NodeBuffer.from(raw, "base64url"))

  private def b64enc(bytes: Chunk[Byte]): String =
    NodeBuffer.from(Buffers.toU8(bytes)).toString("base64url")
end RsaPlatform
