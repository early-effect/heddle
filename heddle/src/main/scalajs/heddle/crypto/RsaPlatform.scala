package heddle.crypto

import heddle.internal.node.{Buffers, Crypto, GenerateKeyOptions, JwkExport, JwkKeyInput, NodeBuffer, SignKeyOptions}
import java.math.BigInteger
import scala.scalajs.js
import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object RsaPlatform:
  def generateSync(bits: Int): RsaKey =
    val pair = Crypto.generateKeyPairSync("rsa", GenerateKeyOptions(bits))
    val jwk  = pair.privateKey.exportJwk(JwkExport("jwk"))
    RsaKey(RsaPublic(b64(jwk.n), b64(jwk.e)), jwk.d.toOption.fold(Chunk.empty[Byte])(b64))

  def signSync(key: RsaKey, payload: Chunk[Byte]): Chunk[Byte] =
    val ko   = Crypto.createPrivateKey(JwkKeyInput(privateJwk(key), "jwk"))
    val opts = SignKeyOptions(ko, padding = Crypto.constants.RSA_PKCS1_PADDING)
    Buffers.fromU8(Crypto.sign("sha256", Buffers.toU8(payload), opts))

  def verifySync(pub: RsaPublic, payload: Chunk[Byte], sigBytes: Chunk[Byte]): Boolean =
    try
      val ko   = Crypto.createPublicKey(JwkKeyInput(publicJwk(pub), "jwk"))
      val opts = SignKeyOptions(ko, padding = Crypto.constants.RSA_PKCS1_PADDING)
      Crypto.verify("sha256", Buffers.toU8(payload), opts, Buffers.toU8(sigBytes))
    catch case _: Exception => false

  def live: ULayer[Rsa] = ZLayer.succeed(Live)

  private object Live extends Rsa:
    def generate(bits: Int): UIO[RsaKey] =
      ZIO.succeed(generateSync(bits))

    def signSha256(key: RsaKey, payload: Chunk[Byte]): UIO[Chunk[Byte]] =
      ZIO.succeed(signSync(key, payload))

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): UIO[Boolean] =
      ZIO.succeed(verifySync(pub, payload, sig))

  /** Node `createPrivateKey` rejects RSA JWKs that only have `n`/`e`/`d`. Recover CRT factors. */
  private def privateJwk(key: RsaKey): js.Dictionary[String] =
    val n      = integer(key.public.n)
    val e      = integer(key.public.e)
    val d      = integer(key.d)
    val (p, q) = recoverPrimes(n, e, d)
    val dp     = d.mod(p.subtract(BigInteger.ONE))
    val dq     = d.mod(q.subtract(BigInteger.ONE))
    val qi     = q.modInverse(p)
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

  private def recoverPrimes(n: BigInteger, e: BigInteger, d: BigInteger): (BigInteger, BigInteger) =
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
    found.getOrElse(throw IllegalArgumentException("could not recover RSA primes"))
  end recoverPrimes

  private def integer(bytes: Chunk[Byte]): BigInteger =
    BigInteger(1, bytes.toArray)

  private def unsigned(n: BigInteger): Chunk[Byte] =
    val raw = n.toByteArray
    if raw.length > 1 && raw(0) == 0 then Chunk.fromArray(raw.tail)
    else Chunk.fromArray(raw)

  private def b64(raw: String): Chunk[Byte] =
    Buffers.fromU8(NodeBuffer.from(raw, "base64url"))

  private def b64enc(bytes: Chunk[Byte]): String =
    NodeBuffer.from(Buffers.toU8(bytes)).toString("base64url")
end RsaPlatform
