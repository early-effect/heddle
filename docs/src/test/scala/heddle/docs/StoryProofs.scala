package heddle.docs

import heddle.*
import heddle.datastar.*
import heddle.docs.fixture.{BoxOffice, DatastarFrames}
import heddle.docs.fixture.Show
import heddle.docs.ui.{DatastarTranscript, HarnessScript, HarnessTranscript, SchemaPicture, SchemaTranscript}
import heddle.endpoint.SchemaDoc
import heddle.http.Url
import heddle.mcp.protocol.{ClientRequest, ToolName}
import zio.*
import zio.json.{DecoderOps, EncoderOps}
import zio.json.ast.Json
import zio.test.*

/** JVM proofs that used to sit on Start / Tutorial pages as `exampleZIO` chrome. */
object StoryProofs extends ZIOSpecDefault:

  def spec = suite("Story proofs")(
    test("GET /health is 200 ok"):
      val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
      routes(Request.get("/health")).map(res => assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "ok"))
    ,
    test("a one-file server answers GET /users/7"):
      val routes = Routes(
        Method.GET / "health"            -> Handler.text("ok"),
        Method.GET / "users" / int("id") -> { id =>
          ZIO.succeed(Response.text(id.toString))
        },
      )
      val app = routes @@ (Middleware.requestId() ++ Middleware.cors())
      app(Request.get("/users/7")).map(res => assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "7"))
    ,
    test("Show schema is named"):
      assertTrue(summon[Schema[Show]].named.contains("Show"))
    ,
    test("getShow doc is GET /shows/{id} promoted get_show"):
      val d = BoxOffice.getShow.doc
      assertTrue(
        d.method == Method.GET,
        d.pathTemplate == "/shows/{id}",
        d.toolName == "get_show",
        d.hints.contains(Hint.ReadOnly),
      )
    ,
    test("HTTP GET /shows/1 is Evening bill"):
      BoxOffice.seed.flatMap { store =>
        BoxOffice.api(store).routes(Request.get("/shows/1")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.text.is(_.some).contains("Evening bill"))
        }
      }
    ,
    test("HTTP GET /shows lists Evening bill"):
      BoxOffice.seed.flatMap { store =>
        BoxOffice.api(store).routes(Request.get("/shows")).map { res =>
          assertTrue(res.status == Status.Ok, res.body.text.is(_.some).contains("Evening bill"))
        }
      }
    ,
    test("HTTP GET /shows/99 is 404"):
      BoxOffice.seed.flatMap { store =>
        val api = BoxOffice.api(store)
        for
          ok   <- api.routes(Request.get("/shows/1"))
          miss <- api.routes(Request.get("/shows/99"))
        yield assertTrue(
          ok.status == Status.Ok,
          ok.body.text.is(_.some).contains("Evening bill"),
          miss.status == Status.NotFound,
        )
      }
    ,
    test("MCP tools/call get_show is Evening bill"):
      BoxOffice.seed.flatMap { store =>
        ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
          val call = BoxOffice.rpc(ClientRequest.CallTool(ToolName("get_show"), Json.Obj("id" -> Json.Num(1))))
          mcp.handle(call).some.map { out =>
            val json = out.toJson
            assertTrue(json.contains("Evening bill"), !json.contains("\"isError\":true"))
          }
        }
      }
    ,
    test("MCP ping completes"):
      BoxOffice.seed.flatMap { store =>
        ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
          mcp.handle(BoxOffice.rpc(ClientRequest.Ping)).some.map { out =>
            assertTrue(out.toJson.contains("\"resultType\":\"complete\""))
          }
        }
      }
    ,
    test("tools/list is the job set"):
      BoxOffice.seed.flatMap { store =>
        ZIO.fromEither(BoxOffice.mcpOf(store)).flatMap { mcp =>
          mcp.handle(BoxOffice.rpc(ClientRequest.ListTools(None))).some.map { out =>
            val json = out.toJson
            assertTrue(
              json.contains("get_show"),
              json.contains("list_shows"),
              json.contains("seat_the_party"),
              !json.contains("create_hold"),
            )
          }
        }
      }
    ,
    test("OpenAPI JSON is a projection of the Api"):
      BoxOffice.seed.map { store =>
        val json = BoxOffice.api(store).openApi.toJson
        assertTrue(json.contains("/shows/{id}"), json.contains("\"get\""), json.contains("One bill"))
      }
    ,
    test("harness transcript is the bound api"):
      for
        fresh <- HarnessScript.load
        store <- BoxOffice.seed
        shows <- store.get
        bill  <- ZIO.fromOption(shows.values.minByOption(_.id)).orElseFail(HarnessScript.Gap.EmptyHouse)
        api = BoxOffice.api(store)
        http   <- api.routes(Request.get(s"/shows/${bill.id}"))
        mcp    <- ZIO.fromEither(BoxOffice.mcpOf(store).flatMap(_.withCatalog))
        listed <- mcp.handle(BoxOffice.rpc(ClientRequest.ListTools(None))).some
      yield
        val docs     = api.ops.map(_.endpoint.doc).toList
        val promoted = docs.filter(doc => doc.promoted && OpArgs.promotable(doc)).map(_.toolName)
        val hidden   = docs.filterNot(doc => doc.promoted && OpArgs.promotable(doc)).map(_.toolName)
        val order    = promoted.map(fresh.list.indexOf)
        val json     = listed.toJson
        assertTrue(
          HarnessTranscript.list == fresh.list,
          HarnessTranscript.call == fresh.call,
          HarnessTranscript.catalogPromoted == fresh.catalogPromoted,
          HarnessTranscript.catalogFull == fresh.catalogFull,
          HarnessTranscript.rpcRequest == fresh.rpcRequest,
          HarnessTranscript.rpcResponse == fresh.rpcResponse,
          HarnessTranscript.rpcKicker == fresh.rpcKicker,
          order.forall(_ >= 0),
          order == order.sorted,
          hidden.forall(fresh.list.contains),
          fresh.list.contains("not promoted"),
          http.status.isSuccess,
          http.body.text.exists(fresh.call.contains),
          fresh.call.contains(s"GET /shows/${bill.id}"),
          fresh.call.contains(BoxOffice.getShow.doc.toolName),
          fresh.call.contains("tools/call"),
          HarnessScript.catalogTools.forall(json.contains),
          promoted.forall(json.contains),
          !json.contains("create_hold"),
        )
    ,
    test("rpc picture is the tools/call wire"):
      for
        fresh <- HarnessScript.load
        store <- BoxOffice.seed
        shows <- store.get
        bill  <- ZIO.fromOption(shows.values.minByOption(_.id)).orElseFail(HarnessScript.Gap.EmptyHouse)
        param <- BoxOffice.getShow.doc.pathParams match
          case only :: Nil => ZIO.succeed(only)
          case _           => ZIO.fail(HarnessScript.Gap.PathArgMissing(BoxOffice.getShow.doc.pathTemplate))
        name <- ZIO
          .fromEither(ToolName.from(BoxOffice.getShow.doc.toolName))
          .mapError(HarnessScript.Gap.BadToolName(_))
        mcp <- ZIO.fromEither(BoxOffice.mcpOf(store)).mapError(HarnessScript.Gap.McpBuild(_))
        call = BoxOffice.rpc(ClientRequest.CallTool(name, Json.Obj(param.name -> Json.Num(bill.id))))
        out <- mcp.handle(call).someOrFail(HarnessScript.Gap.NoReply)
      yield
        val integer = param.schema.unwrapOptional._1 match
          case SchemaDoc.Integer(_) => true
          case _                    => false
        assertTrue(
          integer,
          HarnessTranscript.rpcRequest == fresh.rpcRequest,
          HarnessTranscript.rpcResponse == fresh.rpcResponse,
          HarnessTranscript.rpcKicker == fresh.rpcKicker,
          fresh.rpcRequest == HarnessScript.pretty(call),
          fresh.rpcResponse == HarnessScript.pretty(out),
          fresh.rpcKicker == s"same function as GET /shows/${bill.id}",
          fresh.rpcResponse.contains("Evening bill"),
          fresh.rpcResponse.contains("structuredContent"),
        )
    ,
    test("datastar picture is the sse wire"):
      for
        fresh    <- DatastarFrames.load
        elements <- sseFrame(
          ServerSentEventGenerator.patchElements(
            "<div>hello</div>",
            PatchElementOptions(selector = Some("#app"), mode = ElementPatchMode.Inner),
          )
        )
        signals <- sseFrame(ServerSentEventGenerator.patchSignals("""{"count":1}"""))
      yield assertTrue(
        DatastarTranscript.elements == fresh.elements,
        DatastarTranscript.signals == fresh.signals,
        fresh.elements == elements,
        fresh.signals == signals,
        elements.contains("event: datastar-patch-elements"),
        elements.contains("data: selector #app"),
        elements.contains("data: mode inner"),
        elements.contains("data: elements <div>hello</div>"),
        signals.contains("event: datastar-patch-signals"),
        signals.contains("data: signals {\"count\":1}"),
      )
    ,
    test("schema picture is the Show projections"):
      for
        fresh <- SchemaPicture.load
        store <- BoxOffice.seed
        shows <- store.get
        bill  <- ZIO.fromOption(shows.values.minByOption(_.id)).orElseFail(SchemaPicture.Gap.EmptyHouse)
        api = BoxOffice.api(store)
        http <- api
          .routes(BoxOffice.getShow.toRequest(bill.id, Url.root))
          .mapError(err => SchemaPicture.Gap.ShowNotServed(err.status.render))
        body <- (http.status.isSuccess, http.body.text) match
          case (true, Some(text)) => ZIO.succeed(text)
          case (false, _)         => ZIO.fail(SchemaPicture.Gap.ShowNotServed(http.status.render))
          case (true, None)       => ZIO.fail(SchemaPicture.Gap.NoBody)
        title     <- ZIO.fromOption(summon[Schema[Show]].named).orElseFail(SchemaPicture.Gap.UnnamedShow)
        root      <- ZIO.fromEither(api.openApi.toJson.fromJson[Json]).mapError(SchemaPicture.Gap.OpenApiUnreadable(_))
        component <- schemaComponent(root, title) match
          case Some(schema) => ZIO.succeed(schema)
          case None         => ZIO.fail(SchemaPicture.Gap.NoShowComponent(title))
        tool = BoxOffice.getShow.doc.toolName
        mcp    <- ZIO.fromEither(BoxOffice.mcpOf(store)).mapError(SchemaPicture.Gap.McpBuild(_))
        listed <- mcp.handle(BoxOffice.rpc(ClientRequest.ListTools(None))).someOrFail(SchemaPicture.Gap.NoReply)
        input  <- listedInput(listed, tool) match
          case Some(schema) => ZIO.succeed(schema)
          case None         => ZIO.fail(SchemaPicture.Gap.NoInputSchema(tool))
      yield assertTrue(
        SchemaTranscript.json == fresh.json,
        SchemaTranscript.openApi == fresh.openApi,
        SchemaTranscript.openApiWhere == fresh.openApiWhere,
        SchemaTranscript.mcpArgs == fresh.mcpArgs,
        SchemaTranscript.mcpWhere == fresh.mcpWhere,
        fresh.json == body,
        fresh.json.contains("Evening bill"),
        fresh.openApi == HarnessScript.pretty(component),
        fresh.openApiWhere == s"components.schemas.$title",
        fresh.openApi.contains("remaining"),
        fresh.mcpArgs == HarnessScript.pretty(input),
        fresh.mcpWhere == s"inputSchema for $tool",
        fresh.mcpArgs.contains("\"id\""),
        fresh.mcpArgs.contains("integer"),
      ),
  )

  /** `components.schemas.<name>` from a rendered OpenAPI document. */
  private def schemaComponent(root: Json, name: String): Option[Json] =
    jsonChild(root, "components").flatMap(jsonChild(_, "schemas")).flatMap(jsonChild(_, name))

  /** `inputSchema` of `tool` on a `tools/list` JSON-RPC message. */
  private def listedInput(message: Json, tool: String): Option[Json] =
    jsonChild(message, "result").flatMap(jsonChild(_, "tools")).flatMap {
      case Json.Arr(tools) =>
        tools.collectFirst {
          case entry: Json.Obj if entry.get("name").contains(Json.Str(tool)) => entry.get("inputSchema")
        }.flatten
      case _ => None
    }

  private def jsonChild(json: Json, key: String): Option[Json] =
    json match
      case obj: Json.Obj => obj.get(key)
      case _             => None

  /** One Datastar send, as the SSE response body. Independent of `DatastarFrames`. */
  private def sseFrame(send: URIO[Datastar, Unit]) =
    Routes(Method.GET / "sse" -> events(send)).runZIO(Request.get("/sse")).flatMap { res =>
      if res.status.isSuccess then res.body.utf8.mapError(t => DatastarFrames.Gap.BodyFailed(t.getClass.getName))
      else ZIO.fail(DatastarFrames.Gap.NotServed(res.status.render))
    }
end StoryProofs
