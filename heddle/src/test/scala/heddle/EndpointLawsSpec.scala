package heddle

import zio.*
import zio.test.*

final case class Payload(name: String, count: Int, flags: List[Boolean]) derives Schema, JsonCodec

final case class Probe(id: Int, slug: String, tag: Option[String], n: Int, payload: Payload) derives Schema, JsonCodec

object EndpointLawsSpec extends ZIOSpecDefault:
  private val echo =
    Endpoint
      .post("items" / int("id") / string("slug"))
      .query[Option[String]]("tag")
      .query[Int]("n")
      .in[Payload]
      .as[Probe]
      .out[Probe]

  private val routes = echo.implement(ZIO.succeed(_))

  private val segment = Gen.stringBounded(1, 10)(Gen.printableChar).filter(_ != ".")

  private val probes: Gen[Any, Probe] =
    for
      id    <- Gen.int
      slug  <- Gen.oneOf(segment, Gen.elements("a/b", "x?y", "50%", "é#1", "..", " sp "))
      tag   <- Gen.option(Gen.string)
      n     <- Gen.int
      name  <- Gen.string
      count <- Gen.int
      flags <- Gen.listOfBounded(0, 3)(Gen.boolean)
    yield Probe(id, slug, tag, n, Payload(name, count, flags))

  def spec = suite("Endpoint laws")(
    test("a typed call to an echo endpoint returns its input"):
      check(probes) { p =>
        Client.call(echo)(p).provideLayer(Client.inMemory(routes)).map(out => assertTrue(out == p))
      }
    ,
    test("MCP arguments rebuild a request that decodes to the same input"):
      check(probes) { p =>
        val args = OpArgs.arguments(echo.doc, echo.toRequest(p, Url.root))
        val back = args.flatMap(a => OpArgs.request(echo.doc, a))
        ZIO
          .fromEither(back)
          .flatMap(req => routes(req))
          .flatMap(res => echo.fromResponse(res).mapError(_.toString))
          .map(out => assertTrue(args.isRight, out == p))
      }
    ,
    test("a path argument cannot change the route it lands on"):
      val args = zio.json.ast.Json.Obj(
        "id"    -> zio.json.ast.Json.Num(1),
        "slug"  -> zio.json.ast.Json.Str("../../admin?x=1"),
        "n"     -> zio.json.ast.Json.Num(2),
        "name"  -> zio.json.ast.Json.Str("a"),
        "count" -> zio.json.ast.Json.Num(3),
        "flags" -> zio.json.ast.Json.Arr(),
      )
      val req = OpArgs.request(echo.doc, args)
      assertTrue(
        req.map(_.path.segments) == Right(Chunk("items", "1", "../../admin?x=1")),
        req.map(_.query.get("x")) == Right(None),
      ),
  ) @@ TestAspect.timeout(60.seconds)
end EndpointLawsSpec
