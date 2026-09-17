package heddle

import zio.*
import zio.test.*

object EndpointSpec extends ZIOSpecDefault:
  def spec =
    suite("Endpoint")(
      test("text endpoint implements to a route"):
        val ep     = Endpoint.get("hello").outText()
        val routes = ep.implement(_ => ZIO.succeed("world"))
        routes(Request.get("/hello")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.asString == "world")
        }
      ,
      test("path parameter is the implement input"):
        val ep     = Endpoint.get("echo" / int("n")).outText()
        val routes = ep.implement(n => ZIO.succeed(n.toString))
        routes(Request.get("/echo/4")).map { res =>
          assertTrue(res.body.asString == "4")
        }
      ,
      test("query parameter is combined with the path input"):
        val ep     = Endpoint.get("echo" / int("n")).query[String]("name").outText()
        val routes = ep.implement { case (n, name) => ZIO.succeed(s"$name:$n") }
        routes(Request.get("/echo/2?name=ada")).map { res =>
          assertTrue(res.body.asString == "ada:2")
        }
      ,
      test("json output uses the provided JsonCodec"):
        val ep     = Endpoint.get("msg").out[String]
        val routes = ep.implement(_ => ZIO.succeed("hi"))
        routes(Request.get("/msg")).map { res =>
          assertTrue(
            res.body.asString == "\"hi\"",
            res.header("Content-Type").exists(_.contains("application/json")),
          )
        }
      ,
      test("json input uses the provided JsonCodec"):
        val ep     = Endpoint.post("echo").inJson[String].out[String]
        val routes = ep.implement(body => ZIO.succeed(body))
        routes(Request.post("/echo", Body.json("\"ping\""))).map { res =>
          assertTrue(res.body.asString == "\"ping\"")
        }
      ,
      test("outError maps a typed error to a status"):
        val ep     = Endpoint.get("boom").out[String].outError[String](Status.BadRequest)
        val routes = ep.implement(_ => ZIO.fail("nope"))
        routes(Request.get("/boom")).map { res =>
          assertTrue(res.status == Status.BadRequest, res.body.asString == "\"nope\"")
        }
      ,
      test("header input is combined into implement"):
        val ep     = Endpoint.get("who").header[String]("X-User").outText()
        val routes = ep.implement(name => ZIO.succeed(name))
        routes(Request.get("/who").withHeader("X-User", "ada")).map { res =>
          assertTrue(res.body.asString == "ada")
        }
      ,
      test("inText passes the raw body"):
        val ep     = Endpoint.post("echo").inText.outText()
        val routes = ep.implement(body => ZIO.succeed(body))
        routes(Request.post("/echo", Body.text("ping"))).map { res =>
          assertTrue(res.body.asString == "ping")
        }
      ,
      test("mapIn names a tuple as a product"):
        val ep = Endpoint
          .get("echo" / int("n"))
          .query[String]("name")
          .mapIn { case (n, name) => s"$name:$n" }
          .outText()
        val routes = ep.implement(s => ZIO.succeed(s))
        routes(Request.get("/echo/2?name=ada")).map { res =>
          assertTrue(res.body.asString == "ada:2")
        }
      ,
      test("missing required query is 400"):
        val ep     = Endpoint.get("echo").query[Int]("n").outText()
        val routes = ep.implement(n => ZIO.succeed(n.toString))
        routes(Request.get("/echo")).map { res =>
          assertTrue(res.status == Status.BadRequest)
        }
      ,
      test("Endpoint.get unrolls a static path codec"):
        val path = PathCodec.specialize("echo" / int("n"))
        assertTrue(
          path.specialized,
          path.matches(Path.decode("/echo/4")).contains(4),
          path.matches(Path.decode("/echo/x")).isEmpty,
        )
      ,
      test("specialized nested path params keep Combine nesting"):
        val path = PathCodec.specialize("users" / int("id") / "posts" / int("post"))
        assertTrue(
          path.specialized,
          path.matches(Path.decode("/users/3/posts/9")).contains((3, 9)),
        ),
    ) @@ TestAspect.timeout(5.seconds)
end EndpointSpec
