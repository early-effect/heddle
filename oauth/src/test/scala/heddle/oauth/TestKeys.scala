package heddle.oauth

import heddle.oauth.jose.SigningKey
import zio.*

object TestKeys:
  /** 2048-bit keygen is seconds of CPU under a loaded build. Generate once per suite, off the ZIO workers. */
  def signing(kid: String): ULayer[SigningKey] =
    ZLayer(ZIO.succeedBlocking(SigningKey.generateRsa(kid)))
