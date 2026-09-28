package heddle.mcp.apps.host

import heddle.*
import heddle.error.HeddleError
import heddle.mcp.apps.{Origin, ScriptHash}
import heddle.mcp.apps.ui.{RelayConfig, SandboxGrant}
import java.nio.charset.StandardCharsets
import zio.*
import zio.json.*
import zio.json.ast.Json

/** Why a served relay's URL did not name a grant. The relay server answers 400 and frames nothing. */
enum RelayRequestError(val message: String) extends HeddleError:
  case Missing                   extends RelayRequestError("no r parameter")
  case NotBase64(reason: String) extends RelayRequestError(s"r is not base64url: $reason")
  case Refused(reason: String)   extends RelayRequestError(s"r does not name a grant a relay can honor: $reason")

/** What a served relay's view runs under, as its URL carries it: the clamped grant and the view's script hashes. The
  * server compiles the CSP from this, so a URL can name only a policy the codecs accept, and nothing else.
  */
final case class RelayRequest(grant: SandboxGrant, scripts: Chunk[ScriptHash]):
  def encode: String =
    val json = Json.Obj(
      "grant"   -> grant.toJsonAST.getOrElse(Json.Obj()),
      "scripts" -> Json.Arr(scripts.map(h => Json.Str(h.value))),
    )
    heddle.crypto.Base64Url.encode(Chunk.fromArray(json.toJson.getBytes(StandardCharsets.UTF_8)))

object RelayRequest:
  def of(mount: Mount): RelayRequest =
    RelayRequest(
      mount.grant,
      mount.scripts match
        case Csp.Scripts.Hashed(hs) => hs.toChunk
        case Csp.Scripts.AnyInline  => Chunk.empty,
    )

  /** Strict: anything the codecs would drop fails the whole request. */
  def decode(raw: String): Either[RelayRequestError, RelayRequest] =
    for
      bytes <- heddle.crypto.Base64Url.decode(raw).left.map(e => RelayRequestError.NotBase64(e.message))
      json  <- String(bytes.toArray, StandardCharsets.UTF_8).fromJson[Json.Obj].left.map(RelayRequestError.Refused(_))
      grant <- json.get("grant").toRight("no grant").flatMap(SandboxGrant.strict).left.map(RelayRequestError.Refused(_))
      scripts <- hashes(json.get("scripts").getOrElse(Json.Arr())).left.map(RelayRequestError.Refused(_))
    yield RelayRequest(grant, scripts)

  private def hashes(json: Json): Either[String, Chunk[ScriptHash]] =
    json match
      case Json.Arr(vs) =>
        val parsed = vs.map {
          case Json.Str(s) => ScriptHash.from(s).left.map(e => s"$s: ${e.message}")
          case other       => Left(s"$other is not a script hash")
        }
        parsed.collectFirst { case Left(e) => e }.toLeft(parsed.collect { case Right(h) => h })
      case other => Left(s"scripts is not a list: $other")
end RelayRequest

/** The relay's document, and the headers a served one goes out with. The relay script's bytes never change, so its hash
  * is fixed by the build; what varies is the config element and, when it is `srcdoc`, the CSP `<meta>`.
  */
object RelayDocument:
  /** `</script` inside the bundle would end the element early; `<\/script` means the same to JavaScript. */
  val script: String = RelayScript.text.replace("</script", "<\\/script")

  lazy val scriptHash: ScriptHash = ScriptHash.of(script)

  def csp(request: RelayRequest): Csp = Csp.compile(request.grant, Csp.Scripts.of(request.scripts), scriptHash)

  /** For a static host: the frame writes this as the relay's `srcdoc`, with its CSP as the first element. */
  def opaque(host: Origin, request: RelayRequest): String =
    val policy = csp(request).header.replace("\"", "&quot;")
    html(RelayConfig(host, request.grant), s"""<meta http-equiv="Content-Security-Policy" content="$policy">""")

  /** For the relay server: its CSP goes in the header, with `frame-ancestors` naming the one page that may frame it. */
  def served(host: Origin, request: RelayRequest): Response =
    Response
      .html(html(RelayConfig(host, request.grant), ""))
      .withHeader("Content-Security-Policy", s"${csp(request).header}; frame-ancestors ${host.render}")
      .withHeader("Cache-Control", "no-store")
      .withHeader("Referrer-Policy", "no-referrer")
      .withHeader("X-Content-Type-Options", "nosniff")

  private def html(config: RelayConfig, meta: String): String =
    val data = config.toJson.replace("</", "<\\/")
    s"""<!doctype html><html><head>$meta<meta charset="utf-8">""" +
      s"""<script type="application/json" id="${RelayConfig.ElementId}">$data</script>""" +
      "<style>html,body{margin:0;height:100%;overflow:hidden}</style></head>" +
      s"<body><script>$script</script></body></html>"
end RelayDocument

/** The relay listener: `/sandbox` and nothing else, since anything else on its origin could script the relays. */
object RelayRoutes:
  def apply(host: Origin): Routes[Any, Nothing] =
    Routes(
      Method.GET / "sandbox" -> handler { (req: Request) =>
        ZIO.succeed(
          req.query
            .get("r")
            .toRight(RelayRequestError.Missing)
            .flatMap(RelayRequest.decode)
            .fold(e => Response.badRequest(e.message), RelayDocument.served(host, _))
        )
      }
    )

  /** Where the host page frames a mount's relay: the relay origin, `/sandbox`, and the request. */
  def url(relay: Origin, request: RelayRequest): String = s"${relay.render}/sandbox?r=${request.encode}"
end RelayRoutes
