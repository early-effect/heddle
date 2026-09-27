package heddle.mcp.protocol

import heddle.mcp.protocol.ProtocolGens.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ProtocolLawsSpec extends ZIOSpecDefault:
  private def toolChar(c: Char): Boolean = c.isLetterOrDigit && c < 128 || "_-.".contains(c)

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
