package heddle.mcp.protocol

import heddle.mcp.protocol.ProtocolGens.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ProtocolLawsSpec extends ZIOSpecDefault:
  private def toolChar(c: Char): Boolean = c.isLetterOrDigit && c < 128 || "_-.".contains(c)

  private def subscribeDelivery(headers: Json): Json.Obj =
    Json.Obj(
      "name"     -> Json.Str("tick"),
      "delivery" -> Json.Obj(
        "mode"    -> Json.Str("webhook"),
        "url"     -> Json.Str("https://hooks.example/mcp"),
        "secret"  -> Json.Str("whsec_placeholder"),
        "headers" -> headers,
      ),
    )

  private val idPart = Gen.stringBounded(1, 12)(Gen.oneOf(Gen.alphaNumericChar, Gen.elements('-', '.', '_')))

  private def roundTrips[A: JsonCodec](gen: Gen[Any, A]) =
    check(gen)(a => assertTrue(a.toJsonAST.flatMap(_.as[A]) == Right(a)))

  def spec = suite("MCP wire laws")(
    suite("codecs invert")(
      test("Message")(check(message)(m => assertTrue(Message.decode(m.json) == Right(m)))),
      test("RpcError")(check(rpcError)(e => assertTrue(RpcError.fromJson(e.json).contains(e)))),
      test("ClientRequest")(
        check(clientRequest)(r => assertTrue(ClientRequest.decode(r.method, r.params) == Right(r)))
      ),
      test("a null event cursor is absent and a null ttl does not expire") {
        val params = Json.Obj(
          "name"     -> Json.Str("tick"),
          "delivery" -> Json.Obj(
            "mode"   -> Json.Str("webhook"),
            "url"    -> Json.Str("https://hooks.example/mcp"),
            "secret" -> Json.Str("whsec_placeholder"),
          ),
          "cursor" -> Json.Null,
          "ttlMs"  -> Json.Null,
        )
        assertTrue(
          ClientRequest.decode(Methods.SubscribeEvents, params) == Right(
            ClientRequest.SubscribeEvents(
              EventName("tick"),
              Json.Obj(),
              "https://hooks.example/mcp",
              "whsec_placeholder",
              None,
              None,
              TtlRequest.Unbounded,
              DeliveryHeaders.empty,
            )
          )
        )
      },
      test("delivery.headers is an optional object and a secret") {
        val token   = "Bearer super-secret"
        val params  = subscribeDelivery(Json.Obj("Authorization" -> Json.Str(token)))
        val decoded = ClientRequest.decode(Methods.SubscribeEvents, params)
        val printed = decoded.fold(_.text, _.toString)
        val again   = decoded.flatMap(req => ClientRequest.decode(req.method, req.params))
        assertTrue(
          decoded.exists {
            case ClientRequest.SubscribeEvents(_, _, _, _, _, _, _, headers) =>
              headers.entries == Chunk("Authorization" -> token)
            case _ => false
          },
          !printed.contains(token),
          !printed.contains("super-secret"),
          again == decoded,
        )
      },
      test("delivery.headers rejects reserved names, bad values, and oversize blocks") {
        val token                               = "Bearer super-secret"
        val planted                             = "planted-secret-value"
        def message(delivery: Json.Obj): String =
          ClientRequest.decode(Methods.SubscribeEvents, subscribeDelivery(delivery)) match
            case Left(error) => error.text
            case Right(_)    => ""
        val host     = message(Json.Obj("Host" -> Json.Str(planted)))
        val webhook  = message(Json.Obj("Webhook-Id" -> Json.Str(planted)))
        val mcp      = message(Json.Obj("x-mcp-trace" -> Json.Str(planted)))
        val proxy    = message(Json.Obj("Proxy-Authorization" -> Json.Str(planted)))
        val typed    = message(Json.Obj("Authorization" -> Json.Obj("token" -> Json.Str(planted))))
        val broken   = message(Json.Obj("Authorization" -> Json.Str(s"$token\nmore")))
        val repeated = message(Json.Obj("Authorization" -> Json.Str("a"), "authorization" -> Json.Str(planted)))
        val wide     = message(Json.Obj(Chunk.fromIterable((1 to 17).map(n => s"X-H$n" -> Json.Str("v")))))
        val longName = message(Json.Obj(("A" * 65) -> Json.Str(planted)))
        val longVal  = message(Json.Obj("Authorization" -> Json.Str("a" * 4097)))
        val bulky    = message(
          Json.Obj(Chunk.fromIterable((1 to 16).map(n => s"H$n" -> Json.Str("b" * 600))))
        )
        assertTrue(
          host == "delivery.headers rejects Host",
          webhook == "delivery.headers rejects Webhook-Id",
          mcp == "delivery.headers rejects x-mcp-trace",
          proxy == "delivery.headers rejects Proxy-Authorization",
          typed == "delivery.headers.Authorization must be a string",
          broken == "delivery.headers.Authorization must not contain a control character",
          repeated == "delivery.headers repeats authorization",
          wide == "delivery.headers has 17 names and the cap is 16",
          longName == "delivery.headers has a name longer than 64 characters",
          longVal == "delivery.headers.Authorization is longer than 4096 characters",
          bulky == "delivery.headers exceeds 8192 characters",
          List(host, webhook, mcp, proxy, typed, broken, repeated, wide, longName, longVal, bulky).forall { text =>
            !text.contains(planted) && !text.contains(token) && !text.contains("super-secret")
          },
        )
      },
      test("an unknown callback reason stays an error object") {
        val raw = Json.Obj(
          "code"    -> Json.Num(-32027),
          "message" -> Json.Str("CallbackEndpointError"),
          "data"    -> Json.Obj("reason" -> Json.Str("nope")),
        )
        assertTrue {
          RpcError.fromJson(raw) match
            case Some(RpcError.Other(-32027, "CallbackEndpointError", _)) => true
            case _                                                        => false
        }
      },
      test("Tool")(roundTrips(tool)),
      test("ContentBlock")(roundTrips(content)),
      test("CallToolResult")(roundTrips(callToolResult)),
      test("Resource")(roundTrips(resource)),
      test("ResourceContents")(roundTrips(resourceContents)),
    ),
    suite("decoding is total")(
      test("any JSON is a Message or the Error to answer with"):
        check(ProtocolGens.json) { j =>
          val out = Message.decode(j)
          assertTrue(out.isRight || out.left.exists(_.error.code == -32600))
        }
      ,
      test("any method and params decode to a request or an RpcError"):
        check(text, obj()) { (method, params) =>
          assertTrue(ClientRequest.decode(method, params).fold(_ => true, _ => true))
        }
      ,
      test("an unknown method is MethodNotFound"):
        check(text.filterNot(ClientRequest.known)) { m =>
          assertTrue(ClientRequest.decode(m, Json.Obj()).left.exists(_.code == -32601))
        },
    ),
    suite("structured content")(
      test("every output schema becomes an object schema"):
        check(ProtocolGens.json)(s =>
          assertTrue(Structured.of(s).outputSchema.get("type").contains(Json.Str("object")))
        )
      ,
      test("unwrap inverts wrap for every value the schema admits"):
        check(ProtocolGens.json, ProtocolGens.json, ProtocolGens.obj()) { (schema, any, anObject) =>
          val shape = Structured.of(schema)
          val value = shape match
            case Structured.Direct(_)  => anObject
            case Structured.Wrapped(_) => any
          assertTrue(shape.unwrap(shape.wrap(value)) == value)
        },
    ),
    suite("tool names")(
      test("a literal outside the grammar does not compile"):
        typeCheck("""ToolName("no spaces")""").map(r => assertTrue(r.left.exists(_.contains("not a tool name"))))
      ,
      test("from accepts exactly the grammar"):
        check(Gen.string) { s =>
          val ok = s.nonEmpty && s.length <= 128 && s.forall(toolChar)
          assertTrue(ToolName.from(s).isRight == ok)
        }
      ,
      test("a refused name says which rule it broke"):
        check(Gen.string, Gen.stringBounded(129, 300)(Gen.alphaNumericChar)) { (s, long) =>
          val named = ToolName.from(s) match
            case Left(ToolNameError.BadCharacter(c, at)) =>
              s.lift(at).contains(c) && !toolChar(c) && s.take(at).forall(toolChar)
            case Left(ToolNameError.TooLong(n)) => n == s.length && n > 128
            case Left(ToolNameError.Empty)      => s.isEmpty
            case Right(name)                    => name.value == s
          assertTrue(
            named,
            ToolName.from(long) == Left(ToolNameError.TooLong(long.length)),
            ToolName.from("") == Left(ToolNameError.Empty),
          )
        },
    ),
    suite("cancelled")(
      test("a request id and an optional reason round-trip through the codec"):
        val stop = Cancelled(RequestId.Num(7), Some("stop"))
        val bare = Cancelled(RequestId.Str("a"), None)
        assertTrue(
          stop.toJson.fromJson[Cancelled] == Right(stop),
          bare.toJson.fromJson[Cancelled] == Right(bare),
          stop.message == Message.Notification(Notifications.Cancelled, Cancelled.params(stop)),
          bare.message.params.as[Cancelled] == Right(bare),
        )
      ,
      test("a missing id or a non-string reason is ignored"):
        val missing = Cancelled.from(Json.Obj())
        val badId   = Cancelled.from(Json.Obj("requestId" -> Json.Bool(true)))
        val badWhy  = Cancelled.from(Json.Obj("requestId" -> Json.Num(1), "reason" -> Json.Num(2)))
        assertTrue(missing.isEmpty, badId.isEmpty, badWhy.isEmpty),
    ),
    suite("extension ids")(
      test("prefix/name parses back to its text"):
        check(idPart, idPart)((p, n) => assertTrue(ExtensionId.from(s"$p/$n").map(_.value) == Right(s"$p/$n")))
      ,
      test("anything but exactly one slash between two non-empty parts is refused"):
        check(idPart, idPart) { (p, n) =>
          val shapes = List(p, s"/$n", s"$p/", s"$p/$n/", s"$p//$n", s"$p/$n//", s"/$p/$n")
          assertTrue(shapes.forall(s => ExtensionId.from(s) == Left(ExtensionIdError.NotPrefixSlashName)))
        }
      ,
      test("a bad character is named where it first appears"):
        check(idPart, idPart, Gen.elements(' ', '@', ':', 'é')) { (p, n, bad) =>
          assertTrue(ExtensionId.from(s"$p/$bad$n") == Left(ExtensionIdError.BadCharacter(bad, p.length + 1)))
        },
    ),
  ) @@ TestAspect.timeout(60.seconds)
end ProtocolLawsSpec
