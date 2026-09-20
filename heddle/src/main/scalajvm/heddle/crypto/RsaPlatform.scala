package heddle.crypto

import java.math.BigInteger
import java.security.{KeyFactory, KeyPairGenerator, Signature}
import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}
import java.security.spec.{RSAPrivateKeySpec, RSAPublicKeySpec}
import zio.{Chunk, UIO, ULayer, ZIO, ZLayer}

private[heddle] object RsaPlatform:
  def generateSync(bits: Int): RsaKey =
    val gen = KeyPairGenerator.getInstance("RSA")
    gen.initialize(bits)
    val kp   = gen.generateKeyPair()
    val pub  = kp.getPublic.asInstanceOf[RSAPublicKey]
    val priv = kp.getPrivate.asInstanceOf[RSAPrivateKey]
    RsaKey(
      RsaPublic(unsigned(pub.getModulus), unsigned(pub.getPublicExponent)),
      unsigned(priv.getPrivateExponent),
    )
  end generateSync

  def signSync(key: RsaKey, payload: Chunk[Byte]): Chunk[Byte] =
    val spec = RSAPrivateKeySpec(modulus(key.public.n), integer(key.d))
    val priv = KeyFactory.getInstance("RSA").generatePrivate(spec)
    val sig  = Signature.getInstance("SHA256withRSA")
    sig.initSign(priv)
    sig.update(payload.toArray)
    Chunk.fromArray(sig.sign())

  def verifySync(pub: RsaPublic, payload: Chunk[Byte], sigBytes: Chunk[Byte]): Boolean =
    try
      val spec = RSAPublicKeySpec(modulus(pub.n), integer(pub.e))
      val key  = KeyFactory.getInstance("RSA").generatePublic(spec)
      val sig  = Signature.getInstance("SHA256withRSA")
      sig.initVerify(key)
      sig.update(payload.toArray)
      sig.verify(sigBytes.toArray)
    catch case _: Exception => false

  def live: ULayer[Rsa] = ZLayer.succeed(Live)

  private object Live extends Rsa:
    def generate(bits: Int): UIO[RsaKey] =
      ZIO.succeed(generateSync(bits))

    def signSha256(key: RsaKey, payload: Chunk[Byte]): UIO[Chunk[Byte]] =
      ZIO.succeed(signSync(key, payload))

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): UIO[Boolean] =
      ZIO.succeed(verifySync(pub, payload, sig))

  private def unsigned(n: BigInteger): Chunk[Byte] =
    val raw = n.toByteArray
    if raw.length > 1 && raw(0) == 0 then Chunk.fromArray(raw.tail)
    else Chunk.fromArray(raw)

  private def integer(bytes: Chunk[Byte]): BigInteger =
    BigInteger(1, bytes.toArray)

  private def modulus(bytes: Chunk[Byte]): BigInteger = integer(bytes)
end RsaPlatform
