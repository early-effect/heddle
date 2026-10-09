package heddle

import zio.*
import zio.test.*

object RouteSpec extends ZIOSpecDefault:
  def spec =
    suite("Routes")(
      test("trailing captures remaining path"):
        val routes = Routes(
          Method.GET / trailing -> { path => ZIO.succeed(Response.text(path.render)) }
        )
        for
          root   <- routes.runZIO(Request.get(Url.root))
          nested <- routes.runZIO(Request.get(Url.root / "assets" / "theme.css"))
        yield assertTrue(root.body.text.is(_.some) == "/", nested.body.text.is(_.some) == "/assets/theme.css")
      ,
      test("Server.run merges a matching GET into a response"):
        val routes = Routes(Method.GET / "x" -> Handler.text("ok"))
        Server.run(routes)(Request.get("/x")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "ok")
        }
      ,
      test("literal route wins over trailing"):
        val routes = Routes(
          Method.GET / "sse"    -> Handler.text("sse"),
          Method.GET / trailing -> { _ => ZIO.succeed(Response.text("file")) },
        )
        for
          sse  <- routes.runZIO(Request.get("/sse"))
          file <- routes.runZIO(Request.get("/index.html"))
        yield assertTrue(sse.body.text.is(_.some) == "sse", file.body.text.is(_.some) == "file")
      ,
      test("literal GET returns the handler response"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request.get("/health")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "ok")
        }
      ,
      test("typed path parameter is decoded and passed to the handler"):
        val routes = Routes(
          Method.GET / "users" / int("id") -> { id => ZIO.succeed(Response.text(id.toString)) }
        )
        routes(Request.get("/users/7")).map { res =>
          assertTrue(res.body.text.is(_.some) == "7")
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
        yield assertTrue(a.body.text.is(_.some) == "A", b.body.text.is(_.some) == "B")
      ,
      test("GET and POST on the same path both match after ++"):
        val routes =
          Routes(Method.POST / "users" -> Handler.text("created")) ++
            Routes(Method.GET / "users" -> Handler.text("list"))
        for
          g <- routes(Request.get("/users"))
          p <- routes(Request(Method.POST, Url.parse("/users")))
        yield assertTrue(g.body.text.is(_.some) == "list", p.body.text.is(_.some) == "created", g.status == Status.Ok)
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
          Method.GET / "users" / int("id") / "posts" / int("post") -> { (id, post) =>
            ZIO.succeed(Response.text(s"$id:$post"))
          }
        )
        routes(Request.get("/users/3/posts/9")).map { res =>
          assertTrue(res.body.text.is(_.some) == "3:9")
        }
      ,
      test("string, long, and uuid path codecs decode"):
        import java.util.UUID
        val id     = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        val routes = Routes(
          Method.GET / "s" / string("name") -> { name => ZIO.succeed(Response.text(name)) },
          Method.GET / "n" / long("n")      -> { n => ZIO.succeed(Response.text(n.toString)) },
          Method.GET / "u" / uuid("id")     -> { id => ZIO.succeed(Response.text(id.toString)) },
        )
        for
          s <- routes(Request.get("/s/ada"))
          n <- routes(Request.get("/n/99"))
          u <- routes(Request.get(s"/u/$id"))
        yield assertTrue(
          s.body.text.is(_.some) == "ada",
          n.body.text.is(_.some) == "99",
          u.body.text.is(_.some) == id.toString,
        )
      ,
      test("handler receives path params and the request"):
        val routes = Routes(
          Method.GET / "users" / int("id") handle { (id, req) =>
            val q = req.query.get("q").getOrElse("")
            ZIO.succeed(Response.text(s"$id:$q"))
          }
        )
        routes(Request.get("/users/8?q=hi")).map { res =>
          assertTrue(res.body.text.is(_.some) == "8:hi")
        }
      ,
      test("handleError turns route errors into responses"):
        val routes = Routes(
          Method.GET / "x" -> { _ => ZIO.fail("nope") }
        ).handleError(msg => Response.badRequest(msg))
        routes(Request.get("/x")).map { res =>
          assertTrue(res.status == Status.BadRequest, res.body.text.is(_.some) == "nope")
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
      test("two captures of one type bind in path order") {
        val pattern = Method.GET / "p" / long("left") / "x" / long("right")
        val routes  = Routes(pattern -> { (left, right) => ZIO.succeed(Response.text(s"$left $right")) })
        routes(Request.get("/p/1/x/2")).map { res =>
          assertTrue(res.body.text.is(_.some) == "1 2")
        }
      },
      test("an ignored capture is an underscore") {
        val routes = Routes(
          Method.GET / "p" / long("left") / long("right") -> { (_, right) =>
            ZIO.succeed(Response.text(right.toString))
          }
        )
        routes(Request.get("/p/1/2")).map { res =>
          assertTrue(res.body.text.is(_.some) == "2")
        }
      },
      test("handle takes the captures and then the request") {
        val routes = Routes(
          Method.GET / "p" handle { req => ZIO.succeed(Response.text(req.method.render)) },
          Method.PATCH / "p" / long("left") / long("right") handle { (left, right, req) =>
            ZIO.succeed(Response.text(s"$left $right ${req.method.render}"))
          },
        )
        for
          none <- routes(Request.get("/p"))
          both <- routes(Request(Method.PATCH, Url.parse("/p/1/2")))
        yield assertTrue(none.body.text.is(_.some) == "GET", both.body.text.is(_.some) == "1 2 PATCH")
      },
      test("a swapped capture name does not compile") {
        typeCheck("""
          import heddle.*
          import zio.*
          val _ = Method.GET / "p" / long("left") / long("right") -> { (right, left) =>
            ZIO.succeed(Response.text("no"))
          }
        """).map(compiled => assertTrue(compiled.isLeft))
      },
      test("a tuple parameter does not stand in for the capture names") {
        typeCheck("""
          import heddle.*
          import zio.*
          val _ = Method.GET / "p" / long("left") / long("right") -> { pair =>
            ZIO.succeed(Response.text(pair.toString))
          }
        """).map(compiled => assertTrue(compiled.isLeft))
      },
      test("a case lambda does not bind the captures") {
        typeCheck("""
          import heddle.*
          import zio.*
          val _ = Method.GET / "p" / long("left") / long("right") -> {
            case ((left, right), req) => ZIO.succeed(Response.text(left.toString))
          }
        """).map(compiled => assertTrue(compiled.isLeft))
      },
      test("a path ascribed without its names cannot bind a lambda") {
        typeCheck("""
          import heddle.*
          import zio.*
          val path: PathCodec[Long] = long("left")
          val _ = Method.GET / path -> { left => ZIO.succeed(Response.text(left.toString)) }
        """).map(compiled => assertTrue(compiled.isLeft))
      },
      test("a method reference does not carry capture names") {
        typeCheck("""
          import heddle.*
          import zio.*
          val f: (Long, Long) => ZIO[Any, Nothing, Response] =
            (left, right) => ZIO.succeed(Response.text(s"$left $right"))
          val _ = Method.GET / "p" / long("left") / long("right") -> f
        """).map(compiled => assertTrue(compiled.isLeft))
      },
      test("the same capture name twice does not compose") {
        typeCheck("""
          import heddle.*
          val _ = long("left") / long("left")
        """).map(compiled => assertTrue(compiled.isLeft))
      },
      test("path decode keeps a single interned segment"):
        val codec = PathCodec.lit("health")
        val path  = Path.decode("/health")
        assertTrue(
          path.segments == Chunk("health"),
          path.segments.headOption.exists(_ eq "health"),
          codec.matches(path).contains(()),
        ),
    ) @@ TestAspect.timeout(5.seconds)
end RouteSpec
