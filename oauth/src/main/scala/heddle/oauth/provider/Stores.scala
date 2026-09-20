package heddle.oauth.provider

import heddle.crypto.DigestPlatform
import java.nio.charset.StandardCharsets
import java.time.Instant
import zio.{Chunk, Ref, UIO}

final case class UserRecord(
    id: String,
    username: String,
    passwordHash: String,
    claims: Map[String, String] = Map.empty,
)

final case class ClientRecord(
    id: String,
    secretHash: Option[String],
    redirectUris: List[String],
    grants: Set[String],
)

final case class AuthCode(
    code: String,
    clientId: String,
    userId: String,
    redirectUri: String,
    scopes: Set[String],
    nonce: Option[String],
    codeChallenge: Option[String],
    exp: Instant,
)

final case class RefreshRec(
    token: String,
    clientId: String,
    userId: String,
    scopes: Set[String],
    family: String,
    exp: Instant,
)

final case class SessionRec(id: String, userId: String, exp: Instant)

trait ProviderStores:
  def users: UserStore
  def clients: ClientStore
  def codes: CodeStore
  def tokens: TokenStore
  def sessions: SessionStore

trait UserStore:
  def byUsername(name: String): UIO[Option[UserRecord]]
  def byId(id: String): UIO[Option[UserRecord]]
  def authenticate(username: String, password: String): UIO[Option[UserRecord]]

trait ClientStore:
  def byId(id: String): UIO[Option[ClientRecord]]

trait CodeStore:
  def put(code: AuthCode): UIO[Unit]
  def take(code: String): UIO[Option[AuthCode]]

trait TokenStore:
  def putRefresh(rec: RefreshRec): UIO[Unit]
  def takeRefresh(token: String): UIO[Option[RefreshRec]]

trait SessionStore:
  def put(rec: SessionRec): UIO[Unit]
  def get(id: String): UIO[Option[SessionRec]]

object Passwords:
  def hash(plain: String): String =
    val d = DigestPlatform.sha256Sync(Chunk.fromArray(plain.getBytes(StandardCharsets.UTF_8)))
    "sha256:" + hex(d.toArray)

  private def hex(bytes: Array[Byte]): String =
    val digits = "0123456789abcdef"
    val out    = Array.ofDim[Char](bytes.length * 2)
    var i      = 0
    while i < bytes.length do
      val v = bytes(i) & 0xff
      out(i * 2) = digits.charAt(v >>> 4)
      out(i * 2 + 1) = digits.charAt(v & 0x0f)
      i += 1
    String(out)
  end hex

  def check(plain: String, stored: String): Boolean = hash(plain) == stored
end Passwords

object MemoryStores:
  def unsafeSeed(users: List[UserRecord], clients: List[ClientRecord]): ProviderStores =
    zio.Unsafe.unsafe { implicit u =>
      zio.Runtime.default.unsafe.run(seed(users, clients)).getOrThrowFiberFailure()
    }

  def seed(users: List[UserRecord], clients: List[ClientRecord]): UIO[ProviderStores] =
    for
      u <- Ref.make(users.map(x => x.id -> x).toMap)
      c <- Ref.make(clients.map(x => x.id -> x).toMap)
      k <- Ref.make(Map.empty[String, AuthCode])
      t <- Ref.make(Map.empty[String, RefreshRec])
      s <- Ref.make(Map.empty[String, SessionRec])
    yield new ProviderStores:
      val users = new UserStore:
        def byUsername(name: String)                         = u.get.map(_.values.find(_.username == name))
        def byId(id: String)                                 = u.get.map(_.get(id))
        def authenticate(username: String, password: String) =
          byUsername(username).map(_.filter(rec => Passwords.check(password, rec.passwordHash)))
      val clients = new ClientStore:
        def byId(id: String) = c.get.map(_.get(id))
      val codes = new CodeStore:
        def put(code: AuthCode) = k.update(_ + (code.code -> code))
        def take(code: String)  = k.modify { m => (m.get(code), m - code) }
      val tokens = new TokenStore:
        def putRefresh(rec: RefreshRec) = t.update(_ + (rec.token -> rec))
        def takeRefresh(token: String)  = t.modify { m => (m.get(token), m - token) }
      val sessions = new SessionStore:
        def put(rec: SessionRec) = s.update(_ + (rec.id -> rec))
        def get(id: String)      = s.get.map(_.get(id))
end MemoryStores
