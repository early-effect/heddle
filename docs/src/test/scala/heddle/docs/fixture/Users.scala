package heddle.docs.fixture

import heddle.*
import heddle.json.given
import heddle.mcp.Mcp
import heddle.mcp.protocol.JsonRpc.*
import zio.*
import zio.json.JsonCodec
import zio.json.ast.Json

final case class User(id: Int, name: String) derives Schema, JsonCodec
final case class NewUser(name: String) derives Schema, JsonCodec
final case class NotFound(message: String) derives Schema, JsonCodec

object Users:
  val getUser =
    Endpoint
      .get("users" / int("id"))
      .out[User]
      .outError[NotFound](Status.NotFound)
      .summary("Get a user")
      .tag("users")
      .mcp
      .hints(Hint.ReadOnly)

  val listUsers =
    Endpoint
      .get("users")
      .out[List[User]]
      .summary("List users")
      .tag("users")
      .mcp
      .hints(Hint.ReadOnly)

  val createUser =
    Endpoint
      .post("users")
      .inJson[NewUser]
      .out[User](Status.Created)
      .summary("Create a user")
      .tag("users")

  def seed: UIO[(Ref[Map[Int, User]], Ref[Int])] =
    for
      store  <- Ref.make(Map(1 -> User(1, "Ada")))
      nextId <- Ref.make(2)
    yield (store, nextId)

  def api(store: Ref[Map[Int, User]], nextId: Ref[Int]): Api[Any] =
    Api("Users", "0.1.0")
      .bind(getUser) { id =>
        store.get.map(_.get(id).toRight(NotFound(s"user $id"))).flatMap(ZIO.fromEither)
      }
      .bind(listUsers) { _ =>
        store.get.map(_.values.toList.sortBy(_.id))
      }
      .bind(createUser) { body =>
        nextId.modify(n => n -> (n + 1)).flatMap { id =>
          val user = User(id, body.name)
          store.update(_ + (id -> user)).as(user)
        }
      }

  def mcpOf(store: Ref[Map[Int, User]], nextId: Ref[Int]): Either[String, Mcp[Any]] =
    Mcp.from(api(store, nextId))

  def rpc(method: String, params: Json.Obj, id: Int = 1): Json.Obj =
    val meta = obj(MetaVersion -> Json.Str(ProtocolVersion), MetaClientCaps -> obj())
    val p    = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> p)
end Users
