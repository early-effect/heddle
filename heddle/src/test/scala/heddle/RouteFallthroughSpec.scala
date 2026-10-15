package heddle

import zio.*
import zio.test.*

/** A response a matched route produced is the answer. `++` tries the right side only when the left missed. */
object RouteFallthroughSpec extends ZIOSpecDefault:
  def spec =
    suite("a matched response is not a route miss")(
      test("a typed 404 keeps its JSON body when a later endpoint does not match"):
        followed.routes(Request.post("/orders/1", Body.empty)).map { res =>
          assertTrue(
            res.status == Status.NotFound,
            res.body.text.contains("""{"NotFound":{"id":1}}"""),
          )
        }
      ,
      test("Client.call decodes that 404 when the api has a later endpoint"):
        Client
          .call(ErrorFixtures.order)(1)
          .either
          .provideLayer(Client.inMemory(followed.routes))
          .map(out => assertTrue(out == Left(CallFailure.Domain(OrderError.NotFound(1)))))
      ,
      test("a handler 404 does not run a later route on the same path"):
        for
          leftHits  <- Ref.make(0)
          rightHits <- Ref.make(0)
          left = Routes(
            Method.GET / "p" -> handler(leftHits.update(_ + 1).as(Response.notFound("missing")))
          )
          right = Routes(
            Method.GET / "p" -> handler(rightHits.update(_ + 1).as(Response.text("second")))
          )
          res    <- (left ++ right)(Request.get("/p"))
          leftN  <- leftHits.get
          rightN <- rightHits.get
        yield assertTrue(
          res.status == Status.NotFound,
          res.body.text.contains("missing"),
          leftN == 1,
          rightN == 0,
        )
      ,
      test("a handler 405 keeps its body when a later route allows another method"):
        val denied = Endpoint.post("x").out[String].outError[String](Status.MethodNotAllowed)
        val routes =
          denied.implement(_ => ZIO.fail("nope")) ++
            Routes(Method.GET / "x" -> Handler.text("get"))
        routes(Request.post("/x", Body.empty)).map { res =>
          assertTrue(
            res.status == Status.MethodNotAllowed,
            res.body.text.contains("\"nope\""),
            res.header("Allow").isEmpty,
          )
        }
      ,
      test("requestId keeps a typed 404, header included, when a later route is composed"):
        val routes = (onlyOrder.routes @@ Middleware.requestId()) ++ later
        routes(Request.post("/orders/1", Body.empty)).map { res =>
          assertTrue(
            res.status == Status.NotFound,
            res.body.text.contains("""{"NotFound":{"id":1}}"""),
            res.header("X-Request-Id").exists(_.nonEmpty),
          )
        }
      ,
      test("a path the wrapped api does not serve still reaches the later route"):
        val routes = (onlyOrder.routes @@ Middleware.requestId()) ++ later
        routes(Request.get("/later")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.text.contains("later"))
        }
      ,
      test("provided keeps a typed 404 when a later route does not match"):
        val authed = onlyOrder.routes.provided(_ => ZIO.succeed(Subject("ada")))
        val routes = authed ++ later
        routes(Request.post("/orders/1", Body.empty)).map { res =>
          assertTrue(
            res.status == Status.NotFound,
            res.body.text.contains("""{"NotFound":{"id":1}}"""),
          )
        }
      ,
      test("a long codec miss in one Routes reaches the following string route"):
        val routes = Routes(
          Method.GET / "p" / long("id")     -> { id => ZIO.succeed(Response.text(s"long:$id")) },
          Method.GET / "p" / string("name") -> { name => ZIO.succeed(Response.text(s"str:$name")) },
        )
        routes(Request.get("/p/abc")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.text.contains("str:abc"))
        }
      ,
      test("choose keeps a miss, so a later route still answers"):
        val chosen = Routes.choose { _ =>
          ZIO.succeed(Routes(Method.GET / "a" -> Handler.text("A")))
        }
        val routes = chosen ++ Routes(Method.GET / "b" -> Handler.text("B"))
        for
          a <- routes(Request.get("/a"))
          b <- routes(Request.get("/b"))
        yield assertTrue(a.body.text.contains("A"), b.body.text.contains("B"))
      ,
      test("++ reaches the string route when the long codec misses, and /p/1 runs only the long route"):
        for
          stringHits <- Ref.make(0)
          longRoute = Routes(
            Method.GET / "p" / long("id") -> { id => ZIO.succeed(Response.text(s"long:$id")) }
          )
          stringRoute = Routes(
            Method.GET / "p" / string("name") -> { name =>
              stringHits.update(_ + 1).as(Response.text(s"str:$name"))
            }
          )
          routes = longRoute ++ stringRoute
          missed <- routes(Request.get("/p/abc"))
          hit    <- routes(Request.get("/p/1"))
          hits   <- stringHits.get
        yield assertTrue(
          missed.body.text.contains("str:abc"),
          hit.body.text.contains("long:1"),
          hits == 1,
        ),
    ) @@ TestAspect.timeout(5.seconds)

  /** `order` is not last, so a 404 from it has a right-hand route inside the api. */
  private val followed =
    Api("orders", "1")
      .bind(ErrorFixtures.order)(_ => ZIO.fail(OrderError.NotFound(1)))
      .bind(Endpoint.get("health").out[String])(_ => ZIO.succeed("up"))

  /** One operation, so a typed 404 survives `Api.routes` and the next `++` is what drops it. */
  private val onlyOrder =
    Api("orders", "1").bind(ErrorFixtures.order)(_ => ZIO.fail(OrderError.NotFound(1)))

  private val later =
    Routes(Method.GET / "later" -> Handler.text("later"))

  final case class Subject(name: String)
end RouteFallthroughSpec
