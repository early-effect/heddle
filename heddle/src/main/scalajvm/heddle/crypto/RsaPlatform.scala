package heddle.crypto

import java.math.BigInteger
import java.security.{KeyFactory, KeyPairGenerator, Signature, SignatureException}
import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}
import java.security.spec.{RSAPrivateKeySpec, RSAPublicKeySpec}
import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** JCA RSA. The JCA reports failure by throwing, so each call is attempted here and becomes an `RsaError`. */
private[heddle] object RsaPlatform:
  def live: ULayer[Rsa] = ZLayer.succeed(Live)

  private object Live extends Rsa:
    def generate(bits: Int): IO[RsaError, RsaKey] =
      ZIO
        .attemptBlocking {
          val gen = KeyPairGenerator.getInstance("RSA")
          gen.initialize(bits)
          gen.generateKeyPair()
        }
        .mapError(failed(RsaOperation.Generate))
        .flatMap { kp =>
          (kp.getPublic, kp.getPrivate) match
            case (pub: RSAPublicKey, priv: RSAPrivateKey) =>
              ZIO.succeed(
                RsaKey(
                  RsaPublic(unsigned(pub.getModulus), unsigned(pub.getPublicExponent)),
                  unsigned(priv.getPrivateExponent),
                )
              )
            case _ => ZIO.fail(RsaError.Failed(RsaOperation.Generate, "the provider made a key that is not RSA"))
        }

    def signSha256(key: RsaKey, payload: Chunk[Byte]): IO[RsaError, Chunk[Byte]] =
      ZIO
        .attempt {
          val spec = RSAPrivateKeySpec(integer(key.public.n), integer(key.d))
          val sig  = Signature.getInstance("SHA256withRSA")
          sig.initSign(KeyFactory.getInstance("RSA").generatePrivate(spec))
          sig.update(payload.toArray)
          Chunk.fromArray(sig.sign())
        }
        .mapError(failed(RsaOperation.Sign))

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): IO[RsaError, Boolean] =
      ZIO
        .attempt {
          val key = KeyFactory.getInstance("RSA").generatePublic(RSAPublicKeySpec(integer(pub.n), integer(pub.e)))
          val v   = Signature.getInstance("SHA256withRSA")
          v.initVerify(key)
          v.update(payload.toArray)
          v
        }
        .mapError(failed(RsaOperation.Verify))
        .flatMap { v =>
          // A signature of the wrong length or encoding is one that does not match.
          ZIO.attempt(v.verify(sig.toArray)).catchAll {
            case _: SignatureException => ZIO.succeed(false)
            case other                 => ZIO.fail(failed(RsaOperation.Verify)(other))
          }
        }
  end Live

  private def failed(operation: RsaOperation)(cause: Throwable): RsaError =
    RsaError.Failed(operation, cause.toString)

  private def unsigned(n: BigInteger): Chunk[Byte] =
    val raw = n.toByteArray
    if raw.length > 1 && raw(0) == 0 then Chunk.fromArray(raw.drop(1))
    else Chunk.fromArray(raw)

  private def integer(bytes: Chunk[Byte]): BigInteger =
    BigInteger(1, bytes.toArray)
end RsaPlatform
