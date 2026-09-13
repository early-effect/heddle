package example

import java.nio.file.{Files as JFiles, Path}
import heddle.*
import heddle.json.given
import heddle.sse.*
import zio.*
import zio.json.JsonCodec

final case class User(id: Int, name: String) derives Schema, JsonCodec
final case class NewUser(name: String) derives Schema, JsonCodec
final case class NotFound(message: String) derives Schema, JsonCodec

object Main extends ZIOAppDefault:
  private val getUser =
    Endpoint
      .get("users" / int("id"))
      .out[User]
      .outError[NotFound](Status.NotFound)
      .summary("Get a user")
      .tag("users")

  private val createUser =
    Endpoint
      .post("users")
      .inJson[NewUser]
      .out[User](Status.Created)
      .summary("Create a user")
      .tag("users")

  private val listUsers =
    Endpoint
      .get("users")
      .out[List[User]]
      .summary("List users")
      .tag("users")

  def run =
    for
      store  <- Ref.make(Map(1 -> User(1, "Ada")))
      nextId <- Ref.make(2)
      routes = implemented(store, nextId) ++
        preview ++
        OpenApi.from("Heddle example", "0.1.0", getUser, createUser, listUsers).routes("docs")
      _ <- ZIO.logInfo("listening on http://localhost:8080/docs and http://localhost:8080/preview")
      _ <- Server.sbtInterruptExit
      _ <- Server
        .serve(routes @@ (Middleware.requestId() ++ Middleware.cors() ++ Middleware.debug))
        .provide(Server.Config.defaults)
        .catchAllCause(c => if c.isInterruptedOnly then ZIO.unit else ZIO.refailCause(c))
    yield ()

  private def implemented(store: Ref[Map[Int, User]], nextId: Ref[Int]): Routes[Any, Response] =
    getUser.implement { id =>
      store.get.map(_.get(id).toRight(NotFound(s"user $id"))).flatMap(ZIO.fromEither)
    } ++
      createUser.implement { body =>
        nextId.modify(n => n -> (n + 1)).flatMap { id =>
          val user = User(id, body.name)
          store.update(_ + (id -> user)).as(user)
        }
      } ++
      listUsers.implement { _ =>
        store.get.map(_.values.toList.sortBy(_.id))
      }

  private def preview: Routes[Any, Response] =
    val dir = previewDir
    Routes(
      Method.GET / "preview"                 -> handler(file(dir.resolve("index.html"))),
      Method.GET / "preview" / "preview.css" -> handler(file(dir.resolve("preview.css"))),
      Method.GET / "__preview" / "reload"    -> handler {
        Sse.session { w =>
          w.send(ServerSentEvent("ok", event = Some("reload"))) *> ZIO.never
        }
      },
      Method.GET / "session" -> handler {
        Sse.session { w =>
          w.send(ServerSentEvent("hello"))
        }
      },
    )
  end preview

  private def file(path: Path): UIO[Response] =
    Files.fromPath(path).fold(_ => Response.notFound(), identity)

  /** `sbt example/run` cwd is the example module; a repo-root run still works. */
  private def previewDir: Path =
    val here     = Path.of("preview")
    val fromRoot = Path.of("example", "preview")
    if JFiles.isDirectory(here) then here else fromRoot
end Main
