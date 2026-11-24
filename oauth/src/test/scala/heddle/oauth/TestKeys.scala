package heddle.oauth

import heddle.crypto.{Base64Error, Base64Url, Rsa, RsaError, RsaKey, RsaPublic}
import heddle.oauth.jose.SigningKey
import zio.*

object TestKeys:
  /** 2048-bit keygen is seconds of CPU under a loaded build: generate once per suite. */
  def signing(kid: String): ZLayer[Any, RsaError, SigningKey & Rsa] =
    Rsa.live >+> ZLayer(SigningKey.generateRsa(kid))

  /** This 2048-bit key needs base 17 for CRT recovery, exercising both token signatures without keygen luck. */
  val providerSigning: ZLayer[Any, Base64Error, SigningKey & Rsa] =
    val key = for
      n <- ZIO.fromEither(
        Base64Url.decode(
          "5cnHx7S_GnOGh7XK_-SMSI4twrcXfYV-fblPJI7fhsOTQtjflxLJtzLBw_aK99OQoo1giGAqB8Te0pfhMTF2u1LzCPnSAHU3DEDqsmJxW5381p9nOXaSbzcdXy3jP7QLnG2I3ql0fW2U9MxXJKN3F_EBi5bpLd5oFo4hUJdjxyu7ynn8xV-G96Ls-lJG-3FhCBr7seD3a2TIrQ9PZw2Jc5U6nsMq3wapKGqWYIYI5r6yxoBBt82rcy_VAOhrZ4Xr_2gUgEgLWus3-ANB3N9O4zvS-kaaaglMJOrgRLZ0lqo0VgZU__jPB63w6Kvza89ojpaGNktyjbetLa6ijwBLOQ"
        )
      )
      e <- ZIO.fromEither(Base64Url.decode("AQAB"))
      d <- ZIO.fromEither(
        Base64Url.decode(
          "HFzOuRJ7tg7wBNHbsmsUz6MmTc7d6jiEd43cZsBFzHNZEAVcemAEytp2eogfTgiXOnZ7JZ9tAKl8Rqpyzr4dZXC5yRqrdjKsQ-nCrkL4B_K_sa6vhd23TwfoAFwAcoKiyOuSMHLiYIBFRFPxJky7l0EVoOj_HhIXbz_wJ8DlPWt8xZhW8WETQrBGQh0dWj6JCg24pu9f0jdLjrYWgaarxnmUg5-ZzA4wzB8LQQwJpDIizoS0764bmtKn4e6jLdKA96bs3vZopCTpNBzBubZJ5zAguWfYjnCYxtilTpu2jW0fK6MGJTFVZX2E9sg9g-VmxXRP1adcoUJE7sn8-IY9Ew"
        )
      )
    yield SigningKey("op", RsaKey(RsaPublic(n, e), d))
    Rsa.live >+> ZLayer(key)
end TestKeys
