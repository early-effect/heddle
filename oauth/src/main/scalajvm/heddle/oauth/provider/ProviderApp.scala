package heddle.oauth.provider

import heddle.Server
import heddle.oauth.jose.SigningKey
import zio.*

/** A local OpenID provider with one user (`ada` / `ada`) and two clients, for trying the flows by hand. */
object ProviderApp extends ZIOAppDefault:
  private val config = Server.Config.default.copy(host = "127.0.0.1", port = 8080)

  def run =
    for
      stores <- MemoryStores.seed(
        List(UserRecord("u1", "ada", Passwords.hash("ada"), Map("email" -> "ada@example.test"))),
        List(
          ClientRecord("web", None, List("http://127.0.0.1/cb"), Set("authorization_code", "refresh_token")),
          ClientRecord("machine", Some(Passwords.hash("secret")), Nil, Set("client_credentials")),
        ),
      )
      key <- ZIO.succeedBlocking(SigningKey.generateRsa("op"))
      _   <- Server.sbtInterruptExit
      _   <- Server.serve(Provider.routes(ProviderConfig(issuer = "http://127.0.0.1:8080"), stores, key), config)
    yield ()
end ProviderApp
