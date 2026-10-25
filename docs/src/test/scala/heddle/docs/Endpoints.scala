package heddle.docs

import heddle.*
import heddle.docs.fixture.*
import heddle.docs.ui.Hub
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.json.*
import zio.test.*

object Endpoints extends DocSpecSuite:

  def doc = page("Endpoints and OpenAPI")(
    md"""
`Endpoint` is the typed description of one operation. `Api.bind` attaches the function. OpenAPI
3.1 and Swagger UI are projections. The teaching path is [Operations are an AST](operations-are-an-ast.html)
then [Docs and HTTP fall out](docs-and-http-fall-out.html).
""",
    illustrationIO(Hub.Lives.openApi).live.withMountKey(InteractiveRegistry.EndpointsOpenApi),
    section("The DSL")(
      md"""
`.in[A]`, `.out[A]`, and `.outError[E]` need a `BodyCodec`. A type that `derives Schema, JsonCodec`
gets JSON. A `given BodyCodec[A]` replaces that default. `.query` and `.header` decode inputs.
`.outHeader`, after `.out`, adds a response header to `Out`. The next section is that header.
`.auth`, `.summary`, and `.tag` are documentation that also drives decoding.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          BoxOffice.api(store).routes(Request.get("/shows/1")).flatMap(res => res.body.utf8.map((res.status, _)))
        }
      }.assert { case (status, body) =>
        assertTrue(status == Status.Ok, body.contains("Evening bill"))
      },
    ),
    section("Headers")(
      md"""
A header is a value whose name you pass, or a type that already owns its name.

`HeaderCodec` is the value, and the spellings match a query. `.header[Int]("X-Count")` is missing
as `ParamError.Missing` and `x` as `Malformed`. The same codec has `String`, `Long`, `Boolean`,
`Instant` for `Date`, `Expires`, `Last-Modified`, and `If-Modified-Since`, `Url` for `Location`
(the failure is `ParamError.Url`, the `UrlError` kept whole), and `Chunk[EntityTag]` for
`If-Match` and `If-None-Match`. `.header[Option[Int]]("X-Count")` is absent as `None`. A present
value that does not parse stays an error, and `toRequest` omits `None`. `Content-Length` is
`.header[Long]`, not its own type.

`TypedHeader` owns the name, so the call does not repeat it. `.header[Host]` reads `Host`.
`.header[BasicCredentials]` and `.header[BearerToken]` both read `Authorization` and refuse the
other scheme. `EntityTag` is `ETag`, `RangeSpec` is `Range`. `Cookie`, `Set-Cookie`,
`Content-Encoding`, `Accept-Encoding`, and `Transfer-Encoding` read every line. `Set-Cookie` is
written one line per cookie, never joined. A repeated single-value header, a second `Host`, is
`Malformed`.

`.outHeader` is those two forms on the way out, and it comes after `.out`. The handler returns
the body and the header together. `encodeOut` writes the header, `fromResponse` and `Client.call`
read it back, and a missing one is `CallFailure.Undecodable` with `OutputError.Header`. A later
`.out` replaces that success, header included. OpenAPI documents a request header on the operation
and a response header on the success status only.

A header is not a tool argument, and a response header is not a tool result. `OpArgs.promotable`
is false for an endpoint that declares one, so `Mcp.from` leaves it on HTTP.
""",
      exampleZIO {
        val ep     = Endpoint.get("n").header[Int]("X-Count").header[Option[String]]("X-Trace").out[Text]
        val routes = ep.implement((n, trace) => ZIO.succeed(Text(s"$n:${trace.getOrElse("-")}")))
        val sent   = ep.toRequest((4, None), Url.root)
        for
          good     <- routes(Request.get("/n").withHeader("X-Count", "4").withHeader("X-Trace", "ada"))
          goodBody <- good.body.utf8
          bad      <- routes(Request.get("/n").withHeader("X-Count", "x"))
          badBody  <- bad.body.utf8
          bare     <- routes(Request.get("/n").withHeader("X-Count", "4"))
          bareBody <- bare.body.utf8
        yield (sent.header("X-Count"), sent.header("X-Trace"), good.status, goodBody, bad.status, badBody, bareBody)
      }.assert { case (count, trace, good, goodBody, bad, badBody, bareBody) =>
        assertTrue(
          count.contains("4"),
          trace.isEmpty,
          good == Status.Ok,
          goodBody == "4:ada",
          bad == Status.BadRequest,
          badBody == "header X-Count: expected an int, got 'x'",
          bareBody == "4:-",
        )
      },
      exampleZIO {
        val who    = Endpoint.get("who").header[Host].out[Text]
        val token  = Endpoint.get("me").header[BearerToken].out[Text]
        val people = who.implement(host => ZIO.succeed(Text(host.host))) ++
          token.implement(bearer => ZIO.succeed(Text(bearer.token)))
        for
          host       <- people(Request.get("/who").withHeader("Host", "ada.example"))
          hostBody   <- host.body.utf8
          again      <- people(Request.get("/who").addHeader("Host", "a.example").addHeader("Host", "b.example"))
          bearer     <- people(Request.get("/me").withHeader("Authorization", "Bearer tok"))
          bearerBody <- bearer.body.utf8
          basic      <- people(Request.get("/me").withHeader("Authorization", "Basic xxx"))
          basicBody  <- basic.body.utf8
        yield (
          who.doc.headers.headOption.map(h => (h.name, h.required, h.schema)),
          host.status,
          hostBody,
          again.status,
          bearer.status,
          bearerBody,
          basic.status,
          basicBody,
        )
      }.assert { case (documented, host, hostBody, again, bearer, bearerBody, basic, basicBody) =>
        assertTrue(
          documented.contains(("Host", true, SchemaDoc.Str(None))),
          host == Status.Ok,
          hostBody == "ada.example",
          again == Status.BadRequest,
          bearer == Status.Ok,
          bearerBody == "tok",
          basic == Status.BadRequest,
          basicBody.contains("a bearer authorization"),
        )
      },
      exampleZIO {
        val ep     = Endpoint.get("t").out[Text].outError[String](Status.BadRequest).outHeader[EntityTag]
        val tag    = EntityTag("abc")
        val routes = ep.implement(_ => ZIO.succeed((Text("hi"), tag)))
        for
          res    <- routes(Request.get("/t"))
          back   <- ep.fromResponse(res)
          gone   <- ep.fromResponse(res.removeHeader(HeaderName.ETag)).either
          called <- Client.call(ep)(()).provideLayer(Client.inMemory(routes))
        yield (
          res.header(HeaderName.ETag),
          back,
          gone,
          called,
          ep.doc.responses.exists(r => r.status == Status.Ok && r.headers.exists(_.name == "ETag")),
          ep.doc.responses.exists(r => r.status == Status.BadRequest && r.headers.nonEmpty),
          OpArgs.promotable(ep.doc),
        )
      }.assert { case (etag, back, gone, called, onSuccess, onError, promotable) =>
        assertTrue(
          etag.contains(EntityTag("abc").render),
          back == (Text("hi"), EntityTag("abc")),
          gone == Left(CallFailure.Undecodable(Status.Ok, OutputError.Header("ETag", ParamError.Missing))),
          called == (Text("hi"), EntityTag("abc")),
          onSuccess,
          !onError,
          !promotable,
        )
      },
    ),
    section("OpenAPI is a projection")(
      exampleZIO {
        BoxOffice.seed.map { store =>
          val json = BoxOffice.api(store).openApi.toJson
          json.contains("/shows/{id}") && json.contains("One bill")
        }
      }.assert(ok => assertTrue(ok)),
      md"""
The live projection is on [Docs and HTTP fall out](docs-and-http-fall-out.html).
""",
    ),
    section("Errors")(
      md"""
Model an endpoint's errors as a sealed hierarchy: an `enum`, or a `sealed trait` with case classes
and case objects. `derives Schema, JsonCodec` on the type is all it needs. `.outErrors[E]` pins the
type, then takes one `ErrorCase` per case:

```scala
enum SeatingError derives Schema, JsonCodec:
  case SoldOut(showId: Int)
  case NoBlock(showId: Int, size: Int)

Endpoint
  .post("parties")
  .in[Party]
  .out[PartySeated](Status.Created)
  .outErrors[SeatingError](
    ErrorCase[SeatingError.SoldOut](Status.Conflict),
    ErrorCase[SeatingError.NoBlock](Status.UnprocessableContent),
  )
```

The list is checked against the type at compile time. A missing case, a case listed twice, or a
type that is not a case of `E` does not compile, and the error names the case. Cases may share a
status. A nested `sealed trait` counts as one case covering all of its leaves.

The default body is `JsonCodec[E]`'s encoding: `{"SoldOut":{"showId":1}}` for a case with fields,
`{"Closed":{}}` for a singleton next to cases with fields, and a bare `"Red"` when every case is a
singleton. A `given BodyCodec[E]` replaces that body. The statuses stay. OpenAPI documents each
status with that case's schema. `Client.call` reads the status back to the typed case as
`CallFailure.Domain(case)`, and MCP returns that JSON body as `isError`. Derived `Schema` follows
zio-json's default sum encoding; `@jsonDiscriminator`, `@jsonHint`, or a custom
`JsonCodecConfiguration` change the wire shape without changing the `Schema`.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          val routes              = BoxOffice.api(store).routes
          def seat(party: String) = routes(Request.post("/parties", Body.json(party)))
          for
            soldOut     <- seat("""{"showId":1,"size":13}""")
            soldOutBody <- soldOut.body.utf8
            noBlock     <- seat("""{"showId":3,"size":2}""")
            noBlockBody <- noBlock.body.utf8
          yield (soldOut.status, soldOutBody, noBlock.status, noBlockBody)
        }
      }.assert { case (soldOut, soldOutBody, noBlock, noBlockBody) =>
        assertTrue(
          soldOut == Status.Conflict,
          soldOutBody == """{"SoldOut":{"showId":1}}""",
          noBlock == Status.UnprocessableContent,
          noBlockBody == """{"NoBlock":{"showId":3,"size":2}}""",
        )
      },
      md"""
For a single error type that needs no per-case status, `.outError[E](Status.NotFound)` sends every
error with one status. `OpArgs` is how MCP flattens path, query, and JSON body into tool
arguments; non-JSON bodies are not promotable.
""",
      exampleZIO {
        BoxOffice.seed.flatMap { store =>
          BoxOffice.api(store).routes(Request.get("/shows/99")).map(_.status)
        }
      }.assert(status => assertTrue(status == Status.NotFound)),
    ),
    section("One description, both directions")(
      md"""
An endpoint reads a request into `In` and writes `In` back into a request. Every builder records
both halves, so `toRequest` is total and a typed client needs nothing but the endpoint value.

Inputs are flat. Each builder adds its value to the end, and `Unit` (a literal segment) adds
nothing, so a path id, a query, and a JSON body are an `(Int, Option[String], Seat)`, never nested
pairs. `.as[B]` names that tuple as the case class with the same fields, both ways; `mapIn` takes
any other pair of functions:

```scala
final case class Lookup(id: Int, tag: Option[String])

Endpoint
  .get("items" / int("id"))
  .query[Option[String]]("tag")
  .as[Lookup]
```

The output side is the same. `.out[O]` reads and writes `O` with its `BodyCodec`. `.outHeader`,
after `.out`, appends a response header to that `Out`. A domain type is
JSON. `Text`, `Html`, `Javascript`, `Css`, `Svg`, `Octet`, `Form`, and `EventStream` are the other
bodies, and a `given BodyCodec[A]` is any body you want. `.outEmpty` reads nothing. MCP arguments
are a third view:
`OpArgs.arguments` turns a request into tool arguments and `OpArgs.request` turns them back. The
suite checks both directions as laws over generated inputs, including path values like `a/b` and
`50%`, which travel as one encoded segment and never change the route.
""",
      exampleValue {
        final case class Lookup(id: Int, tag: Option[String])
        val ep = Endpoint.get("items" / int("id")).query[Option[String]]("tag").as[Lookup]
        ep.toRequest(Lookup(7, Some("new")), Url.root).url.render
      }.assert(url => assertTrue(url == "/items/7?tag=new")),
      exampleZIO {
        val ep     = Endpoint.get("items" / int("id")).query[Option[String]]("tag").out[String]
        val routes = ep.implement((id, tag) => zio.ZIO.succeed(s"$id:${tag.getOrElse("-")}"))
        val req    = ep.toRequest((7, Some("a/b")), Url.root)
        for
          args  <- zio.ZIO.fromEither(OpArgs.arguments(ep.doc, req))
          again <- zio.ZIO.fromEither(OpArgs.request(ep.doc, args))
          out   <- routes(again).flatMap(ep.fromResponse)
        yield (req.url.render, args.toJson, out)
      }.assert { case (url, args, out) =>
        assertTrue(url == "/items/7?tag=a%2Fb", args == """{"id":7,"tag":"a/b"}""", out == "7:a/b")
      },
      md"""
A body that streams is not read whole. `Client.call` on an `out[EventStream]` endpoint does not compile;
`Client.subscribe` streams its events and holds the connection for as long as you read.
""",
      expectFail("""Client.call(Endpoint.get("ticks").out[EventStream])(())""").assert { errors =>
        assertTrue(errors.exists(_.message.contains("Read it with Client.subscribe")))
      },
    ),
  )
end Endpoints
