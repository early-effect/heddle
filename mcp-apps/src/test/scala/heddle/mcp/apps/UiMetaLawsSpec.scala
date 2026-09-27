package heddle.mcp.apps

import heddle.mcp.apps.AppGens.*
import heddle.mcp.protocol.ProtocolGens
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object UiMetaLawsSpec extends ZIOSpecDefault:
  private val toolUi: Gen[Any, ToolUi] =
    val uri = Gen.alphaNumericStringBounded(1, 12).map(s => UiUri.from(s"ui://app/$s")).collect { case Right(u) => u }
    (Gen.option(uri) <*> Gen.elements(Visibility.values*)).map(ToolUi(_, _))

  private def obj(raw: String): IO[String, Json.Obj] = ZIO.fromEither(raw.fromJson[Json.Obj])

  def spec = suite("_meta.ui")(
    test("a resource's policy survives encode then decode"):
      check(policy)(p => assertTrue(UiMeta.decodeResource(Some(UiMeta.encodeResource(p))) == (p, Chunk.empty)))
    ,
    test("a tool's view and visibility survive encode then decode"):
      check(toolUi)(t => assertTrue(UiMeta.decodeTool(Some(UiMeta.encodeTool(t))) == (t, Chunk.empty)))
    ,
    test("the same policy is the same bytes, whatever order its origins were added in"):
      check(Gen.listOfBounded(0, 4)(origin)) { os =>
        val forward  = UiPolicy(Network(connect = os.foldLeft(Set.empty[Origin])(_ + _)))
        val backward = UiPolicy(Network(connect = os.reverse.foldLeft(Set.empty[Origin])(_ + _)))
        assertTrue(UiMeta.encodeResource(forward).toJson == UiMeta.encodeResource(backward).toJson)
      }
    ,
    test("decoding any JSON is total: a policy and problems, never a throw"):
      check(ProtocolGens.obj(3)) { o =>
        val (_, _) = UiMeta.decodeResource(Some(o))
        val (_, _) = UiMeta.decodeTool(Some(o))
        assertCompletes
      }
    ,
    test("a wildcard or a path is dropped and reported, never trusted"):
      obj(
        """{"ui":{"csp":{"connectDomains":["https://api.example.com","https://*.cdn.example.com","https://x.example.com/p"]}}}"""
      ).map { meta =>
        val (policy, problems) = UiMeta.decodeResource(Some(meta))
        assertTrue(
          policy.network.connect == Set(Origin("https://api.example.com")),
          problems.length == 2,
          problems.forall {
            case MetaProblem.BadOrigin(Directive.Connect, _, _) => true
            case _                                              => false
          },
        )
      }
    ,
    test("the legacy ui/resourceUri key is read; a missing visibility is both"):
      obj("""{"ui/resourceUri":"ui://w/view"}""").map { meta =>
        assertTrue(
          UiMeta.decodeTool(Some(meta)) == (ToolUi(Some(UiUri("ui://w/view")), Visibility.ModelAndApp), Chunk.empty)
        )
      }
    ,
    test("an unknown permission is reported and not granted"):
      obj("""{"ui":{"permissions":{"camera":{},"teleport":{}}}}""").map { meta =>
        val (policy, problems) = UiMeta.decodeResource(Some(meta))
        assertTrue(
          policy.permissions == Set(Permission.Camera),
          problems == Chunk(MetaProblem.UnknownPermission("teleport")),
        )
      }
    ,
    test("origins parse back from their rendering, and a wildcard never parses"):
      check(origin)(o => assertTrue(Origin.from(o.render) == Right(o))) &&
      check(Gen.alphaNumericStringBounded(1, 8))(s => assertTrue(Origin.from(s"https://*.$s.com").isLeft))
    ,
    test("a default port is dropped, so one origin has one rendering"):
      assertTrue(Origin("HTTPS://API.Example.com:443") == Origin("https://api.example.com"))
    ,
    test("ui:// and origin literals are checked at compile time"):
      (typeCheck("""UiUri("https://not-a-view")""") <*> typeCheck("""Origin("https://*.example.com")""")).map {
        (uri, origin) =>
          assertTrue(uri.left.exists(_.contains("not a ui://")), origin.left.exists(_.contains("not a bare origin")))
      },
  ) @@ TestAspect.timeout(60.seconds)
end UiMetaLawsSpec
