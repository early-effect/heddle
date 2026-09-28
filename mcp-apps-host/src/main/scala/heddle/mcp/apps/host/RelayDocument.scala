package heddle.mcp.apps.host

import heddle.*
import heddle.error.HeddleError
import heddle.mcp.apps.{Origin, ScriptHash}
import heddle.mcp.apps.ui.{RelayConfig, SandboxGrant}
import java.nio.charset.StandardCharsets
import zio.*
import zio.json.*

/** Why a served relay's URL did not name a grant. The relay server answers 400 and frames nothing. */
enum RelayRequestError(val message: String) extends HeddleError:
  case Missing                   extends RelayRequestError("no r parameter")
  case NotBase64(reason: String) extends RelayRequestError(s"r is not base64url: $reason")
  case Refused(reason: String)   extends RelayRequestError(s"r does not name a grant a relay can honor: $reason")

/** What a served relay's view runs under, as its URL carries it: the clamped grant and which inline scripts may run.
  * The server compiles the CSP from this, so a URL can name only a policy the codecs accept, and nothing else.
  */
final case class RelayRequest(grant: SandboxGrant, scripts: Csp.Scripts) derives JsonCodec:
  def encode: String = heddle.crypto.Base64Url.encode(Chunk.fromArray(this.toJson.getBytes(StandardCharsets.UTF_8)))

object RelayRequest:
  /** The server compiles a policy from exactly this, so a grant it cannot honor whole is not a request. */
  private given JsonCodec[SandboxGrant] = SandboxGrant.strict

  def of(mount: Mount): RelayRequest = RelayRequest(mount.grant, mount.scripts)

  def decode(raw: String): Either[RelayRequestError, RelayRequest] =
    heddle.crypto.Base64Url
      .decode(raw)
      .left
      .map(e => RelayRequestError.NotBase64(e.message))
      .flatMap(bytes =>
        String(bytes.toArray, StandardCharsets.UTF_8).fromJson[RelayRequest].left.map(RelayRequestError.Refused(_))
      )
end RelayRequest

/** The relay's document, and the headers a served one goes out with. The relay script's bytes never change, so its hash
  * is fixed by the build; what varies is the config element and, when it is `srcdoc`, the CSP `<meta>`.
  */
object RelayDocument:
  /** The linked relay, with `</script` already written as `<\/script` by the build. */
  val script: String = RelayScript.text

  /** Computed by the build from exactly `script`; `RelayDocumentSpec` checks the two agree. */
  val scriptHash: ScriptHash = RelayScript.hash

  def csp(request: RelayRequest): Csp = Csp.compile(request.grant, request.scripts, scriptHash)

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
