package heddle

import zio.*
import zio.test.*

object RouteSpec extends ZIOSpecDefault:
  def spec =
    suite("Routes")(
      test("trailing captures remaining path"):
        val routes = Routes(
          Method.GET / trailing -> { (path: Path) => ZIO.succeed(Response.text(path.render)) }
        )
        for
          root   <- routes.runZIO(Request.get(Url.root))
          nested <- routes.runZIO(Request.get(Url.root / "assets" / "theme.css"))
        yield assertTrue(root.body.asString == "/", nested.body.asString == "/assets/theme.css")
      ,
      test("literal route wins over trailing"):
        val routes = Routes(
          Method.GET / "sse"    -> Handler.text("sse"),
          Method.GET / trailing -> { (_: Path) => ZIO.succeed(Response.text("file")) },
        )
        for
          sse  <- routes.runZIO(Request.get("/sse"))
          file <- routes.runZIO(Request.get("/index.html"))
        yield assertTrue(sse.body.asString == "sse", file.body.asString == "file")
      ,
      test("literal GET returns the handler response"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request.get("/health")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString == "ok")
        }
      ,
      test("typed path parameter is decoded and passed to the handler"):
        val routes = Routes(
          Method.GET / "users" / int("id") -> { (id: Int) => ZIO.succeed(Response.text(id.toString)) }
        )
        routes(Request.get("/users/7")).map { res =>
          assertTrue(res.body.asString == "7")
        }
      ,
      test("unknown path is 404"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request.get("/missing")).map { res =>
          assertTrue(res.status == Status.NotFound)
        }
      ,
      test("matching path with the wrong method is 405"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request(Method.POST, Url.parse("/health"))).map { res =>
          assertTrue(res.status == Status.MethodNotAllowed, res.header("Allow").contains("GET"))
        }
      ,
      test("routes compose with ++"):
        val routes =
          Routes(Method.GET / "a" -> Handler.text("A")) ++
            Routes(Method.GET / "b" -> Handler.text("B"))
        for
          a <- routes(Request.get("/a"))
          b <- routes(Request.get("/b"))
        yield assertTrue(a.body.asString == "A", b.body.asString == "B")
      ,
      test("GET and POST on the same path both match after ++"):
        val routes =
          Routes(Method.POST / "users" -> Handler.text("created")) ++
            Routes(Method.GET / "users" -> Handler.text("list"))
        for
          g <- routes(Request.get("/users"))
          p <- routes(Request(Method.POST, Url.parse("/users")))
        yield assertTrue(g.body.asString == "list", p.body.asString == "created", g.status == Status.Ok)
      ,
      test("++ merges Allow when neither method matches"):
        val routes =
          Routes(Method.GET / "x" -> Handler.text("g")) ++
            Routes(Method.POST / "x" -> Handler.text("p"))
        routes(Request(Method.PUT, Url.parse("/x"))).map { res =>
          val allow = res.header("Allow").getOrElse("")
          assertTrue(
            res.status == Status.MethodNotAllowed,
            allow.contains("GET"),
            allow.contains("POST"),
          )
        }
      ,
      test("nested path codecs combine parameters"):
        val routes = Routes(
          Method.GET / "users" / int("id") / "posts" / int("post") -> { (pair: (Int, Int)) =>
            val (user, post) = pair
            ZIO.succeed(Response.text(s"$user:$post"))
          }
        )
        routes(Request.get("/users/3/posts/9")).map { res =>
          assertTrue(res.body.asString == "3:9")
        }
      ,
      test("string, long, and uuid path codecs decode"):
        import java.util.UUID
        val id     = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        val routes = Routes(
          Method.GET / "s" / string("name") -> { (name: String) => ZIO.succeed(Response.text(name)) },
          Method.GET / "n" / long("n")      -> { (n: Long) => ZIO.succeed(Response.text(n.toString)) },
          Method.GET / "u" / uuid("id")     -> { (u: UUID) => ZIO.succeed(Response.text(u.toString)) },
        )
        for
          s <- routes(Request.get("/s/ada"))
          n <- routes(Request.get("/n/99"))
          u <- routes(Request.get(s"/u/$id"))
        yield assertTrue(s.body.asString == "ada", n.body.asString == "99", u.body.asString == id.toString)
      ,
      test("handler receives path params and the request"):
        val routes = Routes(
          Method.GET / "users" / int("id") -> { (id: Int, req: Request) =>
            val q = req.query.get("q").getOrElse("")
            ZIO.succeed(Response.text(s"$id:$q"))
          }
        )
        routes(Request.get("/users/8?q=hi")).map { res =>
          assertTrue(res.body.asString == "8:hi")
        }
      ,
      test("handleError turns route errors into responses"):
        val routes = Routes(
          Method.GET / "x" -> { (_: Unit) => ZIO.fail("nope") }
        ).handleError(msg => Response.badRequest(msg))
        routes(Request.get("/x")).map { res =>
          assertTrue(res.status == Status.BadRequest, res.body.asString == "nope")
        }
      ,
      test("compiled path codec matches nested params in one pass"):
        val codec = PathCodec.lit("users") / int("id") / PathCodec.lit("posts") / int("post")
        assertTrue(
          codec.matches(Path.decode("/users/3/posts/9")).contains((3, 9)),
          codec.matches(Path.decode("/users/3/posts")).isEmpty,
          codec.matches(Path.decode("/users/x/posts/9")).isEmpty,
          codec.matches(Path.decode("/users/3/posts/9/extra")).isEmpty,
        )
      ,
      test("empty path codec matches only root"):
        assertTrue(
          PathCodec.empty.matches(Path.root).contains(()),
          PathCodec.empty.matches(Path.decode("/x")).isEmpty,
        )
      ,
      test("path decode keeps a single interned segment"):
        val codec = PathCodec.lit("health")
        val path  = Path.decode("/health")
        assertTrue(
          path.segments == Chunk("health"),
          path.segments.head eq "health",
          codec.matches(path).contains(()),
        ),
    ) @@ TestAspect.timeout(5.seconds)
end RouteSpec
