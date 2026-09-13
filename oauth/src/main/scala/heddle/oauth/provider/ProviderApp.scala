package heddle.oauth.provider

import heddle.Server
import heddle.oauth.jose.SigningKey
import heddle.server.HeddleApp

object ProviderApp extends HeddleApp:
  private val key    = SigningKey.generateRsa("op")
  private val stores = MemoryStores.unsafeSeed(
    List(UserRecord("u1", "ada", Passwords.hash("ada"), Map("email" -> "ada@example.test"))),
    List(
      ClientRecord("web", None, List("http://127.0.0.1/cb"), Set("authorization_code", "refresh_token")),
      ClientRecord("machine", Some(Passwords.hash("secret")), Nil, Set("client_credentials")),
    ),
  )

  def routes = Provider.routes(ProviderConfig(issuer = "http://127.0.0.1:8080"), stores, key)

  override def config: Server.Config = Server.Config.default.copy(host = "127.0.0.1", port = 8080)
end ProviderApp
