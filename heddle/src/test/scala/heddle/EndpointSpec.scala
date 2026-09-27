package heddle

import zio.*
import zio.stream.ZStream
import zio.test.*

final case class Greeting(n: Int, name: String)
final case class Count(n: Int)

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
          .mapIn((n, name) => Greeting(n, name))(g => (g.n, g.name))
          .outText()
        val routes = ep.implement(g => ZIO.succeed(s"${g.name}:${g.n}"))
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
          path.encode((3, 9)).render == "/users/3/posts/9",
        )
      ,
      test("toRequest encodes path query header and json body"):
        val ep =
          Endpoint.post("echo" / int("n")).query[String]("name").header[String]("X-User").inJson[String].out[String]
        val req = ep.toRequest((((4, "ada"), "russ"), "hi"), Url.root)
        assertTrue(
          req.method == Method.POST,
          req.path.render == "/echo/4",
          req.query.get("name").contains("ada"),
          req.header("X-User").contains("russ"),
          req.body.asString == "\"hi\"",
        )
      ,
      test("inMemory Client.call roundtrips an endpoint"):
        val ep     = Endpoint.get("echo" / int("n")).query[String]("name").outText()
        val routes = ep.implement { case (n, name) => ZIO.succeed(s"$name:$n") }
        Client
          .call(ep)((2, "ada"))
          .provideLayer(Client.inMemory(routes))
          .map(out => assertTrue(out == "ada:2"))
      ,
      test("fromResponse decodes JSON success and typed errors"):
        val ep = Endpoint.get("x").out[String].outError[String](Status.BadRequest)
        val ok = ep.encodeOut("hi")
        val no = ep.encodeErr("nope")
        for
          a <- ep.fromResponse(ok)
          b <- ep.fromResponse(no).either
        yield assertTrue(a == "hi", b == Left(CallFailure.Domain("nope")))
      ,
      test("chaining outError keeps only the last status"):
        val ep = Endpoint.get("x").out[String].outError[String](Status.BadRequest).outError[String](Status.Conflict)
        assertTrue(
          ep.encodeErr("nope").status == Status.Conflict,
          !ep.doc.responses.exists(_.status == Status.BadRequest),
          ep.doc.responses.count(_.status == Status.Conflict) == 1,
        )
      ,
      outErrorsSuite,
      test("mapIn round-trips through toRequest, then steps added after it"):
        val ep = Endpoint
          .get("echo" / int("n"))
          .mapIn(Count(_))(_.n)
          .query[String]("name")
          .outText()
        val req = ep.toRequest((Count(4), "ada"), Url.root)
        assertTrue(req.path.render == "/echo/4", req.query.get("name").contains("ada"))
      ,
      test("optional query is omitted when empty"):
        val ep  = Endpoint.get("echo").query[Option[String]]("name").outText()
        val req = ep.toRequest(None, Url.root)
        assertTrue(req.query.get("name").isEmpty, req.path.render == "/echo")
      ,
      test("Client.call does not compile for a streaming endpoint"):
        typeCheck("""Client.call(Endpoint.get("ticks").outSse)(())""").map { result =>
          assertTrue(result.left.exists(_.contains("Client.subscribe")))
        }
      ,
      test("Client.subscribe streams an outSse endpoint's events"):
        val ep     = Endpoint.get("ticks").outSse
        val events = ZStream(heddle.sse.ServerSentEvent("a"), heddle.sse.ServerSentEvent("b"))
        val routes = ep.implement(_ => ZIO.succeed(events))
        Client
          .subscribe(ep)(())
          .runCollect
          .provideLayer(Client.inMemory(routes))
          .map(got => assertTrue(got.map(_.data) == Chunk("a", "b")))
      ,
      test("an outText endpoint reads its body as text, whatever the Content-Type"):
        val ep  = Endpoint.get("t").outText()
        val res =
          Response(Status.Ok).withBody(Body.fromBytes(Chunk.fromArray("hi".getBytes), Some(MediaType.OctetStream)))
        ep.fromResponse(res).map(out => assertTrue(out == "hi"))
      ,
      test("inMemory Client.call roundtrips JSON in and out"):
        val ep     = Endpoint.post("echo").inJson[String].out[String]
        val routes = ep.implement(body => ZIO.succeed(body))
        Client
          .call(ep)("ping")
          .provideLayer(Client.inMemory(routes))
          .map(out => assertTrue(out == "ping")),
    ) @@ TestAspect.timeout(5.seconds)

  private val orderApi =
    Api("orders", "1").bind(ErrorFixtures.order) { id =>
      id match
        case 1 => ZIO.fail(OrderError.NotFound(id))
        case 2 => ZIO.fail(OrderError.Conflict("busy"))
        case 3 => ZIO.fail(OrderError.Unavailable)
        case _ => ZIO.succeed(s"order $id")
    }

  private def post(id: Int) =
    orderApi.routes(Request.post(s"/orders/$id", Body.empty)).flatMap(res => res.body.utf8.map(res.status -> _))

  val outErrorsSuite =
    suite("outErrors")(
      test("each case answers with its own status and the ADT's JSON body"):
        for
          notFound    <- post(1)
          conflict    <- post(2)
          unavailable <- post(3)
          ok          <- post(4)
        yield assertTrue(
          notFound == (Status.NotFound, """{"NotFound":{"id":1}}"""),
          conflict == (Status.Conflict, """{"Conflict":{"reason":"busy"}}"""),
          unavailable == (Status.ServiceUnavailable, """{"Unavailable":{}}"""),
          ok == (Status.Ok, "\"order 4\""),
        )
      ,
      test("fromResponse decodes every case back from its own response"):
        val ep = ErrorFixtures.order
        check(Gen.fromIterable(OrderError.all)) { e =>
          ep.fromResponse(ep.encodeErr(e)).flip.map(back => assertTrue(back == CallFailure.Domain(e)))
        }
      ,
      test("cases may share a status and still decode to the right case"):
        val ep = ErrorFixtures.seating
        check(Gen.fromIterable(List(Seating.SoldOut(1), Seating.NoBlock(1, 2), Seating.Closed))) { e =>
          ep.fromResponse(ep.encodeErr(e)).flip.map(back => assertTrue(back == CallFailure.Domain(e)))
        } && assertTrue(
          ep.encodeErr(Seating.SoldOut(1)).status == Status.Conflict,
          ep.encodeErr(Seating.NoBlock(1, 2)).status == Status.Conflict,
          ep.encodeErr(Seating.Closed).status == Status.Gone,
        )
      ,
      test("an all-singleton enum answers with a plain string body"):
        val ep  = ErrorFixtures.light
        val res = ep.encodeErr(Light.Red)
        ep.fromResponse(res).flip.map { back =>
          assertTrue(
            res.status == Status.Forbidden,
            res.body.asString == "\"Red\"",
            back == CallFailure.Domain(Light.Red),
          )
        }
      ,
      test("a body whose case answers with another status is Undecodable"):
        val res = Response(Status.Conflict).withBody(Body.json("""{"NotFound":{"id":1}}"""))
        ErrorFixtures.order.fromResponse(res).flip.map { failure =>
          assertTrue(failure match
            case CallFailure.Undecodable(Status.Conflict, reason) => reason.contains("answers with 404")
            case _                                                => false)
        }
      ,
      test("a status outside the error set and success is Unexpected"):
        ErrorFixtures.order.fromResponse(Response(Status.BadGateway)).flip.map { failure =>
          assertTrue(failure == CallFailure.Unexpected(Status.BadGateway))
        }
      ,
      test("Client.call surfaces the typed case"):
        Client
          .call(ErrorFixtures.order)(2)
          .either
          .provideLayer(Client.inMemory(orderApi.routes))
          .map(out => assertTrue(out == Left(CallFailure.Domain(OrderError.Conflict("busy")))))
      ,
      test("a nested sealed trait is one case covering all its leaves"):
        val ep = ErrorFixtures.lookup
        check(Gen.fromIterable(List(Lookup.NoUser(1), Lookup.NoOrg(2), Lookup.Throttled))) { e =>
          ep.fromResponse(ep.encodeErr(e)).flip.map(back => assertTrue(back == CallFailure.Domain(e)))
        } && assertTrue(
          ep.encodeErr(Lookup.NoUser(1)).status == Status.NotFound,
          ep.encodeErr(Lookup.NoOrg(2)).status == Status.NotFound,
          ep.encodeErr(Lookup.Throttled).status == Status.TooManyRequests,
          ep.encodeErr(Lookup.NoOrg(2)).body.asString == """{"NoOrg":{"id":2}}""",
        )
      ,
      test("outErrors replaces an earlier outError"):
        val ep = Endpoint
          .get("x")
          .out[String]
          .outError[String](Status.BadRequest)
          .outErrors[Light](ErrorCase[Light.Red.type](Status.Forbidden), ErrorCase[Light.Green.type](Status.Gone))
        assertTrue(ep.doc.responses.map(_.status.code).sorted == List(200, 403, 410)),
    )
end EndpointSpec
