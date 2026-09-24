package heddle.mcp

import heddle.*
import heddle.mcp.protocol.Legacy
import heddle.mcp.protocol.JsonRpc.*
import heddle.mcp.transport.Http
import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

final case class Item(id: Int, name: String) derives Schema, zio.json.JsonCodec
final case class NewItem(name: String) derives Schema, zio.json.JsonCodec
final case class Query(q: String) derives Schema, zio.json.JsonCodec

enum ShopError derives Schema, zio.json.JsonCodec:
  case Missing(id: Int)
  case Closed

object McpSpec extends ZIOSpecDefault:
  private val getItem =
    Endpoint.get("items" / int("id")).out[Item].summary("Get item").mcp.hints(Hint.ReadOnly)
  private val listItems =
    Endpoint.get("items").out[List[Item]].summary("List items").mcp
  private val createItem =
    Endpoint.post("items").inJson[NewItem].out[Item].summary("Create item")

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

  private def mcpOf(store: Ref[Map[Int, Item]]): Mcp[Any] =
    Mcp.from(api(store)).toOption.get

  private def req(method: String, params: Json.Obj, id: Int = 1): Json.Obj =
    val meta = obj(
      MetaVersion    -> Json.Str(ProtocolVersion),
      MetaClientCaps -> obj(),
    )
    val p = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> p)

  private def legacyReq(method: String, params: Json.Obj, id: Int): Json.Obj =
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> params)

  private def postLegacy(
      mcp: Mcp[Any],
      method: String,
      params: Json.Obj,
      id: Int,
      session: Option[String] = None,
      protocol: Option[String] = Some(Legacy.ProtocolVersion),
  ) =
    val req0 = Request.post("/mcp", Body.json(legacyReq(method, params, id).toJson))
    val req1 = protocol.fold(req0)(v => req0.withHeader(Http.ProtocolHeader, v))
    val req  = session.fold(req1)(s => req1.withHeader(Legacy.SessionHeader, s))
    mcp.routes(req)
  end postLegacy

  def spec =
    suite("Mcp")(
      test("server/discover advertises 2026-07-28"):
        for
          store <- Ref.make(Map(1 -> Item(1, "a")))
          out   <- mcpOf(store).handle(req("server/discover", obj()))
        yield
          val json = out.get.toJson
          assertTrue(
            json.contains("2026-07-28"),
            json.contains("\"resultType\":\"complete\""),
            json.contains("Shop"),
          )
      ,
      test("ping returns complete"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          out   <- mcpOf(store).handle(req("ping", obj()))
        yield assertTrue(out.get.toJson.contains("\"resultType\":\"complete\""))
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
          out   <- mcpOf(store).handle(msg)
        yield assertTrue(out.get.toJson.contains("-32022") || out.get.toJson.contains("\"code\":-32022"))
      ,
      test("unknown method is -32601"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          out   <- mcpOf(store).handle(req("nope/nope", obj()))
        yield assertTrue(out.get.toJson.contains("-32601") || out.get.toJson.contains("\"code\":-32601"))
      ,
      test("tools/list is promoted-only by default"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          out   <- mcpOf(store).handle(req("tools/list", obj()))
        yield
          val json = out.get.toJson
          assertTrue(json.contains("get_items_id"), json.contains("get_items"), !json.contains("post_items"))
      ,
      test("job is listed and resource is not"):
        val getItem    = Endpoint.get("items" / int("id")).out[Item].name("get_item").hints(Hint.ReadOnly)
        val createItem = Endpoint.post("items").inJson[NewItem].out[Item].name("create_item")
        val api        = Api("Shop", "1.0.0")
          .resource(createItem)(n => ZIO.succeed(Item(2, n.name)))
          .job(getItem)(id => ZIO.succeed(Item(id, "x")))
        for
          mcp <- ZIO.fromEither(Mcp.from(api))
          out <- mcp.handle(req("tools/list", obj()))
        yield
          val json = out.get.toJson
          assertTrue(json.contains("get_item"), !json.contains("create_item"), !json.contains("post_items"))
      ,
      test("withCatalog adds search_operations and invoke"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp = mcpOf(store).withCatalog
          out <- mcp.handle(req("tools/list", obj()))
        yield
          val json = out.get.toJson
          assertTrue(json.contains("search_operations"), json.contains("invoke"))
      ,
      test("search_operations finds unpromoted ops"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp = mcpOf(store).withCatalog
          out <- mcp.handle(
            req(
              "tools/call",
              obj("name" -> Json.Str("search_operations"), "arguments" -> obj("query" -> Json.Str("create"))),
            )
          )
        yield
          val json = out.get.toJson
          assertTrue(json.contains("post_items"), json.contains("Create item"), !json.contains("\"isError\":true"))
      ,
      test("invoke calls an unpromoted operation"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp = mcpOf(store).withCatalog
          out <- mcp.handle(
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
          val json = out.get.toJson
          assertTrue(json.contains("bob"), !json.contains("\"isError\":true"))
      ,
      test("native tool is callable"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp = mcpOf(store).tool("search_users", "Find users") { (in: Query) =>
            ZIO.succeed(s"hit ${in.q}")
          }
          out <- mcp.handle(
            req("tools/call", obj("name" -> Json.Str("search_users"), "arguments" -> obj("q" -> Json.Str("ada"))))
          )
          listed <- mcp.handle(req("tools/list", obj()))
        yield
          val json = out.get.toJson
          assertTrue(
            json.contains("hit ada"),
            listed.get.toJson.contains("search_users"),
            !json.contains("\"isError\":true"),
          )
      ,
      test("tools/call hits the same function as HTTP"):
        for
          store <- Ref.make(Map(1 -> Item(1, "ada")))
          mcp = mcpOf(store)
          out <- mcp.handle(
            req("tools/call", obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(1))))
          )
          http <- api(store).routes(Request.get("/items/1"))
        yield
          val json = out.get.toJson
          assertTrue(json.contains("ada"), http.body.asString.contains("ada"), !json.contains("\"isError\":true"))
      ,
      test("HTTP Mcp-Name mismatch is 400 / -32020"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp  = mcpOf(store)
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
          res.body.asString.contains("-32020") || res.body.asString.contains("\"code\":-32020"),
        )
      ,
      test("GET /mcp is 405"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          res   <- mcpOf(store).routes(Request.get("/mcp"))
        yield assertTrue(res.status == Status.MethodNotAllowed)
      ,
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
          _     <- mcpOf(store).stdio(in, out)
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
          mcp  = mcpOf(store)
          body = req("ping", obj()).toJson
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "tools/list")
          )
        yield assertTrue(
          res.status == Status.BadRequest,
          res.body.asString.contains("-32020") || res.body.asString.contains("\"code\":-32020"),
        )
      ,
      test("HTTP discover with matching headers is 200"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp  = mcpOf(store)
          body = req("server/discover", obj()).toJson
          res <- mcp.routes(
            Request
              .post("/mcp", Body.json(body))
              .withHeader(Http.ProtocolHeader, ProtocolVersion)
              .withHeader(Http.MethodHeader, "server/discover")
          )
        yield assertTrue(res.status == Status.Ok, res.body.asString.contains("2026-07-28"))
      ,
      test("stdio framing round-trips ping"):
        val inBytes = (req("ping", obj()).toJson + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
        val in      = java.io.ByteArrayInputStream(inBytes)
        val out     = java.io.ByteArrayOutputStream()
        for
          store <- Ref.make(Map.empty[Int, Item])
          _     <- mcpOf(store).stdio(in, out)
        yield assertTrue(String(out.toByteArray).contains("complete"))
      ,
      test("protected resource metadata is unauthenticated"):
        val routes = Mcp.protectedResource("http://localhost:8080/mcp", List("http://localhost:8080"), List("openid"))
        routes(Request.get("/.well-known/oauth-protected-resource")).map { res =>
          assertTrue(
            res.status == Status.Ok,
            res.body.asString.contains("authorization_servers"),
            res.body.asString.contains("/mcp"),
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
        val mcp  = Mcp.from(api).toOption.get
        mcp
          .handle(req("tools/call", obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(9)))))
          .map { out =>
            val json = out.get.toJson
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
        Mcp
          .from(api)
          .toOption
          .get
          .handle(req("tools/call", obj("name" -> Json.Str("get_items_id"), "arguments" -> obj("id" -> Json.Num(9)))))
          .map { out =>
            val json = out.get.toJson
            assertTrue(json.contains("\"isError\":true"), json.contains("""{\"Missing\":{\"id\":9}}"""))
          }
      ,
      test("missing bearer on protected MCP is 401 with resource_metadata"):
        val meta = "http://localhost:8080/.well-known/oauth-protected-resource"
        Ref.make(Map.empty[Int, Item]).flatMap { store =>
          val locked = mcpOf(store).routes.provided(Mcp.bearer(meta, List("openid"))(_ => ZIO.succeed("ok")))
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
          res   <- postLegacy(
            mcpOf(store),
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
          val body = res.body.asString
          assertTrue(
            res.status == Status.Ok,
            body.contains("2025-11-25"),
            body.contains("Shop"),
            !body.contains("resultType"),
            res.header(Legacy.SessionHeader).exists(_.nonEmpty),
          )
      ,
      test("HTTP 2025 list and call reuse Engine without 2026 headers"):
        for
          store <- Ref.make(Map(1 -> Item(1, "ada")))
          mcp = mcpOf(store)
          init <- postLegacy(
            mcp,
            "initialize",
            obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
            1,
            protocol = None,
          )
          sid = init.header(Legacy.SessionHeader)
          ack  <- postLegacy(mcp, "notifications/initialized", obj(), 2, session = sid)
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
          val listed = list.body.asString
          val called = call.body.asString
          assertTrue(
            ack.status == Status.Accepted,
            list.status == Status.Ok,
            listed.contains("get_items_id"),
            listed.contains("get_items"),
            !listed.contains("resultType"),
            !listed.contains("ttlMs"),
            call.status == Status.Ok,
            called.contains("ada"),
            !called.contains("resultType"),
            http.body.asString.contains("ada"),
            list.header(Legacy.SessionHeader) == sid,
          )
      ,
      test("DELETE /mcp is 200"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          res   <- mcpOf(store).routes(Request(Method.DELETE, Url.parse("/mcp")))
        yield assertTrue(res.status == Status.Ok)
      ,
      test("modern discover still works on the same Mcp as initialize"):
        for
          store <- Ref.make(Map.empty[Int, Item])
          mcp = mcpOf(store)
          _ <- postLegacy(
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
        yield assertTrue(res.status == Status.Ok, res.body.asString.contains("2026-07-28"))
      ,
      test("stdio initialize then list and call without _meta"):
        val lines =
          List(
            legacyReq(
              "initialize",
              obj("protocolVersion" -> Json.Str(Legacy.ProtocolVersion), "capabilities" -> obj()),
              1,
            ).toJson,
            legacyReq("notifications/initialized", obj(), 2).toJson,
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
            _     <- mcpOf(store).stdio(in, out)
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
