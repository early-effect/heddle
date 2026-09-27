package heddle.oauth

import heddle.crypto.{Rsa, RsaError}
import heddle.oauth.jose.SigningKey
import zio.*

object TestKeys:
  /** 2048-bit keygen is seconds of CPU under a loaded build: generate once per suite. */
  def signing(kid: String): ZLayer[Any, RsaError, SigningKey & Rsa] =
    Rsa.live >+> ZLayer(SigningKey.generateRsa(kid))
