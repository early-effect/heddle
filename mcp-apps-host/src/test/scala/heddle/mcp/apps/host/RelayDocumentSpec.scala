package heddle.mcp.apps.host

import heddle.*
import heddle.mcp.apps.{AppGens, Origin, ScriptHash}
import heddle.mcp.apps.ui.{RelayConfig, SandboxGrant}
import java.nio.charset.StandardCharsets
import zio.*
import zio.json.*
import zio.test.*

object RelayDocumentSpec extends ZIOSpecDefault:
  private val host = Origin("http://127.0.0.1:4000")

  private val request: Gen[Any, RelayRequest] =
    for
      network <- AppGens.network
      perms   <- AppGens.permissions
      scripts <- Gen.listOfBounded(0, 3)(Gen.alphaNumericString)
    yield RelayRequest(SandboxGrant(network, perms), Chunk.fromIterable(scripts.map(ScriptHash.of)))

  private def raw(json: String): String =
    heddle.crypto.Base64Url.encode(Chunk.fromArray(json.getBytes(StandardCharsets.UTF_8)))

  /** The text between an element's opening tag and its end tag. */
  private def between(html: String, open: String, close: String): Option[String] =
    html.indexOf(open) match
      case -1 => None
      case at =>
        val from = at + open.length
        html.indexOf(close, from) match
          case -1  => None
          case end => Some(html.substring(from, end))

  private def get(r: Option[String]): UIO[Response] =
    RelayRoutes(host).runZIO(Request.get(r.fold("/sandbox")(v => s"/sandbox?r=$v")))

  def spec = suite("RelayDocument")(
    test("a relay request survives its URL encoding"):
      check(request)(r => assertTrue(RelayRequest.decode(r.encode) == Right(r)))
    ,
    test("a request naming anything a relay cannot honor is refused whole"):
      val hash = ScriptHash.of("view").value
      val bad  = List(
        """{"grant":{"csp":{"connectDomains":["https://*.evil.test"]}}}""",
        """{"grant":{"permissions":{"telepathy":{}}}}""",
        s"""{"grant":{},"scripts":["$hash","'unsafe-inline'"]}""",
        """{"grant":{},"scripts":"sha256-x"}""",
        """{"scripts":[]}""",
        """not json""",
      )
      assertTrue(
        bad.forall(b => RelayRequest.decode(raw(b)).left.exists(_.isInstanceOf[RelayRequestError.Refused])),
        RelayRequest.decode("%%%").left.exists(_.isInstanceOf[RelayRequestError.NotBase64]),
      )
    ,
    test("a served relay carries exactly the compiled policy, framed only by its host, and is never cached"):
      check(request) { r =>
        get(Some(r.encode)).map { res =>
          assertTrue(
            res.status == Status.Ok,
            res
              .header("Content-Security-Policy")
              .contains(s"${RelayDocument.csp(r).header}; frame-ancestors ${host.render}"),
            res.header("Cache-Control").contains("no-store"),
            res.header("Referrer-Policy").contains("no-referrer"),
            res.header("X-Content-Type-Options").contains("nosniff"),
          )
        }
      }
    ,
    test("a served relay with no request, or a bad one, is a 400 with no document"):
      for
        missing <- get(None)
        bad     <- get(Some(raw("""{"grant":{"csp":{"frameDomains":["*"]}}}""")))
      yield assertTrue(
        missing.status == Status.BadRequest,
        bad.status == Status.BadRequest,
        missing.header("Content-Security-Policy").isEmpty,
      )
    ,
    test("an opaque relay opens with its CSP, then its config, and runs the one script its CSP hashes"):
      check(request) { r =>
        val html     = RelayDocument.opaque(host, r)
        val config   = between(html, s"""id="${RelayConfig.ElementId}">""", "</script>")
        val script   = between(html, "<body><script>", "</script></body>")
        val runnable = if r.scripts.isEmpty then "'unsafe-inline'" else RelayDocument.scriptHash.source
        assertTrue(
          html.startsWith(s"""<!doctype html><html><head><meta http-equiv="Content-Security-Policy" content="""),
          config.map(_.replace("<\\/", "</").fromJson[RelayConfig]).contains(Right(RelayConfig(host, r.grant))),
          script.contains(RelayDocument.script),
          script.map(ScriptHash.of).contains(RelayDocument.scriptHash),
          RelayDocument.csp(r).header.split("; ").exists(d => d.startsWith("script-src ") && d.contains(runnable)),
        )
      }
    ,
    test("the relay's script cannot end its own element early"):
      assertTrue(!RelayDocument.script.contains("</script"), RelayDocument.script.nonEmpty),
  ) @@ TestAspect.timeout(120.seconds)
end RelayDocumentSpec
