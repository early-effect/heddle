package heddle.crypto

import heddle.internal.openssl.{OpenSslRsa, SslError}
import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

private[heddle] object RsaPlatform:
  def live: ULayer[Rsa] = ZLayer.succeed(Live)

  private object Live extends Rsa:
    def generate(bits: Int): IO[RsaError, RsaKey] =
      ZIO
        .blocking(ZIO.suspendSucceed(ZIO.fromEither(OpenSslRsa.generate(bits))))
        .mapBoth(
          failed(RsaOperation.Generate),
          (n, e, d) => RsaKey(RsaPublic(Chunk.fromArray(n), Chunk.fromArray(e)), Chunk.fromArray(d)),
        )

    def signSha256(key: RsaKey, payload: Chunk[Byte]): IO[RsaError, Chunk[Byte]] =
      ZIO
        .suspendSucceed(
          ZIO.fromEither(OpenSslRsa.sign(key.public.n.toArray, key.public.e.toArray, key.d.toArray, payload.toArray))
        )
        .mapBoth(failed(RsaOperation.Sign), Chunk.fromArray)

    def verifySha256(pub: RsaPublic, payload: Chunk[Byte], sig: Chunk[Byte]): IO[RsaError, Boolean] =
      ZIO
        .suspendSucceed(ZIO.fromEither(OpenSslRsa.verify(pub.n.toArray, pub.e.toArray, payload.toArray, sig.toArray)))
        .mapError(failed(RsaOperation.Verify))
  end Live

  private def failed(operation: RsaOperation)(e: SslError): RsaError = RsaError.Failed(operation, e.message)
end RsaPlatform
