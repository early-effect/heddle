package heddle.mcp

import heddle.*
import heddle.sse.SseCodec
import heddle.mcp.protocol.{
  CallToolResult,
  ContentBlock,
  ExtensionId,
  ListToolsResult,
  ReadResourceResult,
  RequestMeta,
  Resource,
  ResourceContents,
}
import heddle.mcp.server.ToolCall
import heddle.mcp.transport.Http
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

final case class Item(id: Int, name: String) derives Schema, zio.json.JsonCodec
final case class NewItem(name: String) derives Schema, zio.json.JsonCodec
final case class Query(q: String) derives Schema, zio.json.JsonCodec

enum ShopError derives Schema, zio.json.JsonCodec:
  case Missing(id: Int)
  case Closed

object McpSpec extends ZIOSpecDefault:
  private def obj(fields: (String, Json)*): Json.Obj = Json.Obj(fields*)

  private val ProtocolVersion = heddle.mcp.protocol.ProtocolVersion.Current.value
  private val MetaVersion     = RequestMeta.VersionKey
  private val MetaClientCaps  = RequestMeta.ClientCapsKey

  private object Legacy:
    val ProtocolVersion = heddle.mcp.protocol.ProtocolVersion.Legacy.value
    val SessionHeader   = Http.SessionHeader

  private val getItem =
    Endpoint.get("items" / int("id")).out[Item].summary("Get item").mcp.hints(Hint.ReadOnly)
  private val listItems =
    Endpoint.get("items").out[List[Item]].summary("List items").mcp
  private val createItem =
    Endpoint.post("items").in[NewItem].out[Item].summary("Create item")

  private def api(store: Ref[Map[Int, Item]]): Api[Any] =
    Api("Shop", "1.0.0")
      .bind(getItem) { id =>
        store.get.map(_.get(id).toRight("missing")).flatMap {
          case Left(_)  => ZIO.succeed(Item(id, "missing"))
          case Right(i) => ZIO.succeed(i)
        }
      }
      .bind(listItems) { _ => store.get.map(_.values.toList.sortBy(_.id)) }
      .bind(createItem) { n => ZIO.succeed(Item(2, n.name)) }

  /** What a test does not expect: a server that does not build, or a request that gets no answer. */
  private enum Unexpected:
    case Build(errors: NonEmptyChunk[McpBuildError])
    case NoAnswer
    case Undecodable(json: String)

  /** A server the test expects to build; a build error fails the test with its cases. */
  private def built[R](out: Either[NonEmptyChunk[McpBuildError], Mcp[R]]): IO[Unexpected, Mcp[R]] =
    ZIO.fromEither(out).mapError(Unexpected.Build(_))

  private def mcpOf(store: Ref[Map[Int, Item]]): IO[Unexpected, Mcp[Any]] =
    built(Mcp.from(api(store)))

  private val shop: Either[NonEmptyChunk[McpBuildError], Mcp[Any]] = Mcp.from(Api("Shop", "1.0.0"))

  extension [R](mcp: Mcp[R])
    /** The JSON answer to one request. A request that gets no answer fails the test. */
    private def answer(msg: Json): ZIO[R, Unexpected, String] =
      mcp.handle(msg).someOrFail(Unexpected.NoAnswer).map(_.toJson)

  /** `tools/call` through the engine, read back as the typed result. */
  private def callResult(mcp: Mcp[Any], tool: String, args: Json.Obj): IO[Unexpected, CallToolResult] =
    mcp.answer(req("tools/call", obj("name" -> Json.Str(tool), "arguments" -> args))).flatMap { json =>
      ZIO
        .fromOption(
          json.fromJson[Json.Obj].toOption.flatMap(_.get("result")).flatMap(_.toJson.fromJson[CallToolResult].toOption)
        )
        .orElseFail(Unexpected.Undecodable(json))
    }

  private def req(method: String, params: Json.Obj, id: Int = 1): Json.Obj =
    val meta = obj(
      MetaVersion    -> Json.Str(ProtocolVersion),
      MetaClientCaps -> obj(),
    )
    val p = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> p)

  private def legacyReq(method: String, params: Json.Obj, id: Int): Json.Obj =
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> params)

  /** A notification has no id (JSON-RPC 2.0 §4.1). */
  private def legacyNote(method: String): Json.Obj =
    obj("jsonrpc" -> Json.Str("2.0"), "method" -> Json.Str(method), "params" -> obj())

  private def postLegacy(
      mcp: Mcp[Any],
      method: String,
      params: Json.Obj,
      id: Int,
      session: Option[String] = None,
      protocol: Option[String] = Some(Legacy.ProtocolVersion),
  ) =
    postLegacyJson(mcp, legacyReq(method, params, id), session, protocol)

  private def postLegacyJson(
      mcp: Mcp[Any],
      msg: Json.Obj,
      session: Option[String],
      protocol: Option[String] = Some(Legacy.ProtocolVersion),
  ) =
    val req0 = Request.post("/mcp", Body.json(msg.toJson))
    val req1 = protocol.fold(req0)(v => req0.withHeader(Http.ProtocolHeader, v))
    val req  = session.fold(req1)(s => req1.withHeader(Legacy.SessionHeader, s))
    mcp.routes(req)
  end postLegacyJson

  def spec =
    suite("Mcp")(
      test("server/discover advertises 2026-07-28"):
        for
          store <- Ref.make(Map(1 -> Item(1, "a")))
          mcp   <- mcpOf(store)
          out   <- mcp.answer(req("server/discover", obj()))
        yield
          val json = out
          assertTrue(
            json.contains("2026-07-28"),
            json.contains("\"resultType\":\"complete\""),
            json.contains("Shop"),
          )
      ,
      test("ping returns complete"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          out   <- mcp.answer(req("ping", obj()))
        yield assertTrue(out.contains("\"resultType\":\"complete\""))
      ,
      test("unsupported version is -32022"):
        val meta = obj(MetaVersion -> Json.Str("1900-01-01"), MetaClientCaps -> obj())
        val msg  = obj(
          "jsonrpc" -> Json.Str("2.0"),
          "id"      -> Json.Num(1),
          "method"  -> Json.Str("ping"),
          "params"  -> obj("_meta" -> meta),
        )
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          out   <- mcp.answer(msg)
        yield assertTrue(out.contains("-32022") || out.contains("\"code\":-32022"))
      ,
      test("unknown method is -32601"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          out   <- mcp.answer(req("nope/nope", obj()))
        yield assertTrue(out.contains("-32601") || out.contains("\"code\":-32601"))
      ,
      test("tools/list is promoted-only by default"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          out   <- mcp.answer(req("tools/list", obj()))
        yield
          val json = out
          assertTrue(json.contains("get_items_id"), json.contains("get_items"), !json.contains("post_items"))
      ,
      test("job is listed and resource is not"):
        val getItem    = Endpoint.get("items" / int("id")).out[Item].name("get_item").hints(Hint.ReadOnly)
        val createItem = Endpoint.post("items").in[NewItem].out[Item].name("create_item")
        val api        = Api("Shop", "1.0.0")
          .resource(createItem)(n => ZIO.succeed(Item(2, n.name)))
          .job(getItem)(id => ZIO.succeed(Item(id, "x")))
        for
          mcp <- built(Mcp.from(api))
          out <- mcp.answer(req("tools/list", obj()))
        yield
          val json = out
          assertTrue(json.contains("get_item"), !json.contains("create_item"), !json.contains("post_items"))
      ,
      test("a JSON job is a tool and an HTML page stays on HTTP"):
        val getItem = Endpoint.get("items" / int("id")).out[Item].name("get_item")
        val page    = Endpoint.get("page").out[Html].name("home_page")
        val api     = Api("Shop", "1.0.0")
          .job(getItem)(id => ZIO.succeed(Item(id, "x")))
          .job(page)(_ => ZIO.succeed(Html("<p>hi</p>")))
        val refused = ToolCall.bound(page.implement(_ => ZIO.succeed(Html("<p>hi</p>"))))
        for
          mcp    <- built(Mcp.from(api))
          listed <- mcp.answer(req("tools/list", obj()))
          http   <- api.routes(Request.get("/page")).either
        yield assertTrue(
          listed.contains("get_item"),
          !listed.contains("home_page"),
          http.exists { res =>
            res.status == Status.Ok &&
            res.header("Content-Type").exists(_.contains("text/html")) &&
            res.body.text.contains("<p>hi</p>")
          },
          refused match
            case Left(McpBuildError.NotPromotable("home_page", OpArgsError.NonJsonSuccess)) => true
            case _                                                                          => false,
        )
        end for
      ,
      test("withCatalog adds search_operations and invoke"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store).flatMap(m => built(m.withCatalog))
          out   <- mcp.answer(req("tools/list", obj()))
        yield
          val json = out
          assertTrue(json.contains("search_operations"), json.contains("invoke"))
      ,
      test("search_operations finds unpromoted ops"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store).flatMap(m => built(m.withCatalog))
          out   <- mcp.answer(
            req(
              "tools/call",
              obj("name" -> Json.Str("search_operations"), "arguments" -> obj("query" -> Json.Str("create"))),
            )
          )
        yield
          val json = out
          assertTrue(json.contains("post_items"), json.contains("Create item"), !json.contains("\"isError\":true"))
      ,
      test("invoke calls an unpromoted operation"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store).flatMap(m => built(m.withCatalog))
          out   <- mcp.answer(
            req(
              "tools/call",
              obj(
                "name"      -> Json.Str("invoke"),
                "arguments" -> obj(
                  "operationId" -> Json.Str("post_items"),
                  "arguments"   -> obj("name" -> Json.Str("bob")),
                ),
              ),
            )
          )
        yield
          val json = out
          assertTrue(json.contains("bob"), !json.contains("\"isError\":true"))
      ,
      test("native tool is callable"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store).flatMap(m =>
            built(m.tool[Query]("search_users", "Find users")(in => ZIO.succeed(s"hit ${in.q}")))
          )
          out <- mcp.answer(
            req("tools/call", obj("name" -> Json.Str("search_users"), "arguments" -> obj("q" -> Json.Str("ada"))))
          )
          listed <- mcp.answer(req("tools/list", obj()))
        yield
          val json = out
          assertTrue(
            json.contains("hit ada"),
            listed.contains("search_users"),
            !json.contains("\"isError\":true"),
          )
      ,
      test("tools/call hits the same function as HTTP"):
        for
          store <- Ref.make(Map(1 -> Item(1, "ada")))
          mcp   <- mcpOf(store)
          out   <- mcp.answer(
            req("tools/call", obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(1))))
          )
          http <- api(store).routes(Request.get("/items/1"))
        yield
          val json = out
          assertTrue(
            json.contains("ada"),
            http.body.text.is(_.some).contains("ada"),
            !json.contains("\"isError\":true"),
          )
      ,
      test("HTTP Mcp-Name mismatch is 400 / -32020"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          body = req(
            "tools/call",
            obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(1))),
          ).toJson
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "tools/call")
              .withHeader(Http.NameHeader, "nope")
          )
        yield assertTrue(
          res.status == Status.BadRequest,
          res.body.text.is(_.some).contains("-32020") || res.body.text.is(_.some).contains("\"code\":-32020"),
        )
      ,
      test("GET /mcp without a session is 400"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          res   <- mcp.routes(Request.get("/mcp"))
        yield assertTrue(
          res.status == Status.BadRequest,
          res.body.text.is(_.some).contains("session id"),
        )
      ,
      test("a session hears resources/updated, and a stateless subscribe is refused") {
        for
          store <- Ref.make(Map.empty[Int, Item])
          plain <- mcpOf(store)
          mcp   <- built(plain.withResources(ServedResource.text(Resource("notes://board", "board"), "[]")))
          init  <- postLegacy(
            mcp,
            "initialize",
            obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
            1,
            protocol = None,
          )
          id <- ZIO.fromOption(init.header(Legacy.SessionHeader)).orElseFail(Unexpected.NoAnswer)
          deniedBody = req("resources/subscribe", obj("uri" -> Json.Str("notes://board")), 9).toJson
          denied <- mcp.routes(
            Request
              .post("/mcp", Body.json(deniedBody))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "resources/subscribe")
          )
          sub <- postLegacy(
            mcp,
            "resources/subscribe",
            obj("uri" -> Json.Str("notes://board")),
            2,
            session = Some(id),
          )
          stream <- mcp.routes(
            Request
              .get("/mcp")
              .withHeader(Legacy.SessionHeader, id)
              .withHeader("Accept", "text/event-stream")
          )
          heard <- SseCodec.stream(stream.body.toStream).take(2).runCollect.fork
          _     <- ZIO.iterate((0, 0))((n, i) => n < 1 && i < 40) { case (_, i) =>
            mcp.listeners(id).zipLeft(ZIO.sleep(5.millis)).map(n => (n, i + 1))
          }
          _      <- mcp.resourceUpdated("notes://board")
          _      <- mcp.toolsChanged
          events <- heard.join
          text = events.map(_.data).mkString("\n")
        yield assertTrue(
          denied.status == Status.BadRequest,
          denied.body.text.is(_.some).contains("needs a session"),
          sub.status == Status.Ok,
          stream.status == Status.Ok,
          text.contains("notifications/resources/updated"),
          text.contains("notes://board"),
          text.contains("notifications/tools/list_changed"),
        )
      } @@ TestAspect.withLiveClock,
      test("stdio discover list and call round-trip"):
        val lines =
          List(
            req("server/discover", obj(), 1).toJson,
            req("tools/list", obj(), 2).toJson,
            req(
              "tools/call",
              obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(1))),
              3,
            ).toJson,
          ).mkString("", "\n", "\n")
        val in  = java.io.ByteArrayInputStream(lines.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        val out = java.io.ByteArrayOutputStream()
        for
          store <- Ref.make(Map(1 -> Item(1, "ada")))
          mcp   <- mcpOf(store)
          _     <- mcp.stdio(in, out)
        yield
          val text = String(out.toByteArray)
          assertTrue(
            text.contains("2026-07-28"),
            text.contains("get_items_id"),
            text.contains("ada"),
          )
        end for
      ,
      test("HTTP header mismatch is 400 / -32020"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          body = req("ping", obj()).toJson
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "tools/list")
          )
        yield assertTrue(
          res.status == Status.BadRequest,
          res.body.text.is(_.some).contains("-32020") || res.body.text.is(_.some).contains("\"code\":-32020"),
        )
      ,
      test("HTTP discover with matching headers is 200"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          body = req("server/discover", obj()).toJson
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "server/discover")
          )
        yield assertTrue(res.status == Status.Ok, res.body.text.is(_.some).contains("2026-07-28"))
      ,
      test("stdio framing round-trips ping"):
        val inBytes = (req("ping", obj()).toJson + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
        val in      = java.io.ByteArrayInputStream(inBytes)
        val out     = java.io.ByteArrayOutputStream()
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          _     <- mcp.stdio(in, out)
        yield assertTrue(String(out.toByteArray).contains("complete"))
      ,
      test("protected resource metadata is unauthenticated"):
        val routes = Mcp.protectedResource("http://localhost:8080/mcp", List("http://localhost:8080"), List("openid"))
        routes(Request.get("/.well-known/oauth-protected-resource")).map { res =>
          assertTrue(
            res.status == Status.Ok,
            res.body.text.is(_.some).contains("authorization_servers"),
            res.body.text.is(_.some).contains("/mcp"),
          )
        }
      ,
      test("MCP unauthorized includes resource_metadata"):
        val res = Mcp.unauthorized("http://localhost:8080/.well-known/oauth-protected-resource", List("openid"))
        assertTrue(
          res.status == Status.Unauthorized,
          res.header("WWW-Authenticate").exists(_.contains("resource_metadata")),
        )
      ,
      test("typed Err is isError, not a JSON-RPC error"):
        val boom = Endpoint.get("items" / int("id")).out[Item].outError[String](Status.NotFound).mcp
        val api  = Api("Shop", "1.0.0").bind(boom) { _ => ZIO.fail("gone") }
        built(Mcp.from(api))
          .flatMap(
            _.answer(
              req("tools/call", obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(9))))
            )
          )
          .map { json =>
            assertTrue(json.contains("\"isError\":true"), json.contains("gone"), !json.contains("\"error\""))
          }
      ,
      test("outErrors payload is the error ADT's JSON body"):
        val lookup = Endpoint
          .get("items" / int("id"))
          .out[Item]
          .outErrors[ShopError](
            ErrorCase[ShopError.Missing](Status.NotFound),
            ErrorCase[ShopError.Closed.type](Status.ServiceUnavailable),
          )
          .mcp
        val api = Api("Shop", "1.0.0").bind(lookup)(id => ZIO.fail(ShopError.Missing(id)))
        built(Mcp.from(api))
          .flatMap(
            _.answer(
              req("tools/call", obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(9))))
            )
          )
          .map { json =>
            assertTrue(json.contains("\"isError\":true"), json.contains("""{\"Missing\":{\"id\":9}}"""))
          }
      ,
      test("a typed error travels as structuredContent a client can decode, beside its text"):
        val lookup = Endpoint
          .get("items" / int("id"))
          .out[Item]
          .outErrors[ShopError](
            ErrorCase[ShopError.Missing](Status.NotFound),
            ErrorCase[ShopError.Closed.type](Status.ServiceUnavailable),
          )
          .mcp
        val api = Api("Shop", "1.0.0").bind(lookup)(id => ZIO.fail(ShopError.Missing(id)))
        built(Mcp.from(api)).flatMap(callResult(_, "get_items_id", obj("id" -> Json.Num(9)))).map { r =>
          val back = r.structuredContent.flatMap(_.get("value")).flatMap(_.as[ShopError].toOption)
          assertTrue(r.failed, back.contains(ShopError.Missing(9)))
        }
      ,
      test("a non-object output is wrapped as value, and its outputSchema is an object"):
        val greet = Endpoint.get("greet" / string("who")).out[String].mcp
        val api   = Api("Shop", "1.0.0").bind(greet)(who => ZIO.succeed(s"hi $who"))
        for
          mcp    <- built(Mcp.from(api))
          r      <- callResult(mcp, "get_greet_who", obj("who" -> Json.Str("ada")))
          listed <- mcp.answer(req("tools/list", obj()))
        yield
          val tools  = listed.fromJson[Json.Obj].toOption.flatMap(_.get("result"))
          val schema = tools
            .flatMap(_.toJson.fromJson[ListToolsResult].toOption)
            .flatMap(_.tools.headOption)
            .flatMap(_.outputSchema)
          assertTrue(
            r.structuredContent.contains(obj("value" -> Json.Str("hi ada"))),
            r.content == Chunk(ContentBlock.Text("hi ada")),
            schema.flatMap(_.get("type")).contains(Json.Str("object")),
          )
        end for
      ,
      test("a native tool's own error is typed; one that cannot fail needs no codec"):
        val tools = shop
          .flatMap(
            _.tool[Query]("strict_search", "Fails on empty")(q =>
              if q.q.isEmpty then ZIO.fail(ShopError.Closed) else ZIO.succeed(List(q.q))
            )
          )
          .flatMap(_.tool[Query]("echo")(q => ZIO.succeed(q.q)))
        for
          mcp  <- built(tools)
          bad  <- callResult(mcp, "strict_search", obj("q" -> Json.Str("")))
          good <- callResult(mcp, "echo", obj("q" -> Json.Str("ada")))
        yield assertTrue(
          bad.failed,
          bad.structuredContent.flatMap(_.get("value")).flatMap(_.as[ShopError].toOption).contains(ShopError.Closed),
          !good.failed,
          good.structuredContent.contains(obj("value" -> Json.Str("ada"))),
        )
        end for
      ,
      test("a message with an id is a request, even when its method looks like a notification"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          asReq <- mcp.handle(req("notifications/initialized", obj()))
          note  <- mcp.handle(obj("jsonrpc" -> Json.Str("2.0"), "method" -> Json.Str("notifications/x")))
        yield assertTrue(asReq.exists(_.toJson.contains("-32601")), note.isEmpty)
      ,
      test("a tool name outside the grammar is a build error, not a runtime surprise"):
        val weird = Endpoint.get("items" / int("id")).out[Item].mcp("get item!")
        val out   = Mcp.from(Api("Shop", "1.0.0").bind(weird)(id => ZIO.succeed(Item(id, "x"))))
        assertTrue(out.left.exists(_.exists {
          case McpBuildError.InvalidToolName("get item!", _) => true
          case _                                             => false
        }))
      ,
      test("a tool named like one the server has is a build error, not a silent overwrite"):
        Ref.make(Map.empty[Int, Item]).flatMap(mcpOf).map { mcp =>
          val clash = mcp.tool[Query]("get_items_id")(q => ZIO.succeed(q.q))
          assertTrue(clash.left.exists(_.exists {
            case McpBuildError.DuplicateTool(n) => n.value == "get_items_id"
            case _                              => false
          }))
        }
      ,
      test("the catalog refuses to shadow an operation named invoke"):
        val invoke = Endpoint.get("run").out[String].mcp("invoke")
        val api    = Api("Shop", "1.0.0").bind(invoke)(_ => ZIO.succeed("mine"))
        assertTrue(
          Mcp
            .from(api)
            .flatMap(_.withCatalog)
            .left
            .exists(_.exists {
              case McpBuildError.DuplicateTool(n) => n.value == "invoke"
              case _                              => false
            })
        )
      ,
      test("resources are listed, read, and advertised; an unknown uri is -32002"):
        val page = Resource("ui://shop/board", "board", mimeType = Some("text/html;profile=mcp-app"))
        for
          mcp    <- built(shop.flatMap(_.withResources(ServedResource.text(page, "<p>hi</p>"))))
          disc   <- mcp.handle(req("server/discover", obj()))
          listed <- mcp.handle(req("resources/list", obj()))
          read   <- mcp.handle(req("resources/read", obj("uri" -> Json.Str("ui://shop/board"))))
          miss   <- mcp.handle(req("resources/read", obj("uri" -> Json.Str("ui://shop/nope"))))
        yield
          val contents = read
            .flatMap(_.toJson.fromJson[Json.Obj].toOption)
            .flatMap(_.get("result"))
            .flatMap(_.toJson.fromJson[ReadResourceResult].toOption)
            .map(_.contents)
          assertTrue(
            disc.exists(_.toJson.contains("\"subscribe\":true")),
            listed.exists(_.toJson.contains("ui://shop/board")),
            contents.contains(Chunk(ResourceContents.Text("ui://shop/board", page.mimeType, "<p>hi</p>", None))),
            miss.exists(_.toJson.contains("-32002")),
          )
        end for
      ,
      test("a resource that cannot be read now is an internal error, not a crash"):
        val flaky = ServedResource(Resource("ui://shop/flaky", "flaky"), ZIO.fail(ResourceUnavailable("disk")))
        for
          mcp <- built(shop.flatMap(_.withResources(flaky)))
          out <- mcp.handle(req("resources/read", obj("uri" -> Json.Str("ui://shop/flaky"))))
        yield assertTrue(out.exists(_.toJson.contains("-32603")), out.exists(_.toJson.contains("disk")))
      ,
      test("two resources with one uri are a build error"):
        val a = ServedResource.text(Resource("ui://shop/a", "a"), "1")
        val b = ServedResource.text(Resource("ui://shop/a", "b"), "2")
        assertTrue(
          shop
            .flatMap(_.withResources(a, b))
            .left
            .exists(_.exists {
              case McpBuildError.DuplicateResource("ui://shop/a") => true
              case _                                              => false
            })
        )
      ,
      test("an extension is advertised in discover and in the 2025 initialize"):
        for
          mcp <- built(
            shop.map(
              _.withExtension(ExtensionId.Ui, obj("mimeTypes" -> Json.Arr(Json.Str("text/html;profile=mcp-app"))))
            )
          )
          disc <- mcp.handle(req("server/discover", obj()))
          init <- postLegacy(
            mcp,
            "initialize",
            obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
            1,
            protocol = None,
          )
          body <- init.body.utf8
        yield assertTrue(
          disc.exists(_.toJson.contains(""""extensions":{"io.modelcontextprotocol/ui":{"mimeTypes"""")),
          body.contains("io.modelcontextprotocol/ui"),
        )
        end for
      ,
      test("a native tool name outside the grammar does not compile"):
        typeCheck("""Mcp.from(Api("S", "1")).flatMap(_.tool[Query]("no spaces")(q => ZIO.succeed(q.q)))""").map { r =>
          assertTrue(r.left.exists(_.contains("not a tool name")))
        }
      ,
      test("missing bearer on protected MCP is 401 with resource_metadata"):
        val meta = "http://localhost:8080/.well-known/oauth-protected-resource"
        Ref.make(Map.empty[Int, Item]).flatMap(mcpOf).flatMap { mcp =>
          val locked = mcp.routes.provided(Mcp.bearer(meta, List("openid"))(_ => ZIO.succeed("ok")))
          val body   = req("ping", obj()).toJson
          locked(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "ping")
          ).map { res =>
            assertTrue(
              res.status == Status.Unauthorized,
              res.header("WWW-Authenticate").exists(_.contains("resource_metadata")),
            )
          }
        }
      ,
      test("HTTP initialize is 2025-11-25 with a session id"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          res   <- postLegacy(
            mcp,
            "initialize",
            obj(
              "protocolVersion" -> Json.Str(Legacy.ProtocolVersion),
              "capabilities"    -> obj(),
              "clientInfo"      -> obj("name" -> Json.Str("test"), "version" -> Json.Str("1")),
            ),
            1,
            protocol = None,
          )
        yield
          val body = res.body.text
          assertTrue(
            res.status == Status.Ok,
            body.is(_.some).contains("2025-11-25"),
            body.is(_.some).contains("Shop"),
            !body.is(_.some).contains("resultType"),
            res.header(Legacy.SessionHeader).exists(_.nonEmpty),
          )
      ,
      test("HTTP 2025 list and call reuse Engine without 2026 headers"):
        for
          store <- Ref.make(Map(1 -> Item(1, "ada")))
          mcp   <- mcpOf(store)
          init  <- postLegacy(
            mcp,
            "initialize",
            obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
            1,
            protocol = None,
          )
          sid = init.header(Legacy.SessionHeader)
          ack  <- postLegacyJson(mcp, legacyNote("notifications/initialized"), session = sid)
          list <- postLegacy(mcp, "tools/list", obj(), 3, session = sid)
          call <- postLegacy(
            mcp,
            "tools/call",
            obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(1))),
            4,
            session = sid,
          )
          http <- api(store).routes(Request.get("/items/1"))
        yield
          val listed = list.body.text
          val called = call.body.text
          assertTrue(
            ack.status == Status.Accepted,
            list.status == Status.Ok,
            listed.is(_.some).contains("get_items_id"),
            listed.is(_.some).contains("get_items"),
            !listed.is(_.some).contains("resultType"),
            !listed.is(_.some).contains("ttlMs"),
            call.status == Status.Ok,
            called.is(_.some).contains("ada"),
            !called.is(_.some).contains("resultType"),
            http.body.text.is(_.some).contains("ada"),
            list.header(Legacy.SessionHeader) == sid,
          )
      ,
      test("DELETE /mcp is 200"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          res   <- mcp.routes(Request(Method.DELETE, Url.parse("/mcp")))
        yield assertTrue(res.status == Status.Ok)
      ,
      test("modern discover still works on the same Mcp as initialize"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp   <- mcpOf(store)
          _     <- postLegacy(
            mcp,
            "initialize",
            obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
            1,
            protocol = None,
          )
          body = req("server/discover", obj()).toJson
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "server/discover")
          )
        yield assertTrue(res.status == Status.Ok, res.body.text.is(_.some).contains("2026-07-28"))
      ,
      test("stdio initialize then list and call without _meta"):
        val lines =
          List(
            legacyReq(
              "initialize",
              obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
              1,
            ).toJson,
            legacyNote("notifications/initialized").toJson,
            legacyReq("tools/list", obj(), 3).toJson,
            legacyReq(
              "tools/call",
              obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(1))),
              4,
            ).toJson,
          ).mkString("", "\n", "\n")
        val in  = java.io.ByteArrayInputStream(lines.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        val out = java.io.ByteArrayOutputStream()
        val ran =
          for
            store <- Ref.make(Map(1 -> Item(1, "ada")))
            mcp   <- mcpOf(store)
            _     <- mcp.stdio(in, out)
          yield
            val text = String(out.toByteArray)
            assertTrue(
              text.contains("2025-11-25"),
              text.contains("get_items_id"),
              text.contains("ada"),
              !text.contains("resultType"),
            )
        ran,
    ) @@ TestAspect.timeout(10.seconds)
end McpSpec
