package heddle.apps.browser

import chekhov.*
import chekhov.driver.PlaywrightDriver
import heddle.*
import heddle.error.ServerError
import heddle.mcp.apps.{Origin, OriginError}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** The S2 spike from `heddle-mcp-apps-design.md` §7: what the MCP Apps double-iframe sandbox assumes of a browser.
  *
  * A host page frames three relays, and each relay frames a view as `srcdoc` with `sandbox="allow-scripts"` and no
  * policy of its own. The view runs a probe the relay's CSP allows by hash, holds a script it does not, fetches the
  * host page's `/ping` (which answers any origin), and reads `window.top`, then posts what happened through the relay.
  *
  * The two relays under test carry the policy the two ways the host kit will: in its own response header (Served), or
  * as the first element of a `srcdoc` relay's head (Opaque). The control relay carries none, so everything the view
  * cannot do under the other two, it can do there, and the relay's CSP is the only difference.
  */
object CspInheritanceSpec extends ZIOSpecDefault:
  /** Where a relay carries its CSP. */
  enum Csp:
    case Header, Meta, Absent

  enum Fetch derives JsonDecoder:
    case Resolved, Rejected

  /** What one view saw. `topLocation` is set only if the view could read the top page's location. */
  final case class Report(
      origin: String,
      topThrows: Option[String],
      topLocation: Option[String],
      fetch: Fetch,
      unhashedRan: Boolean,
      violations: List[String],
  ) derives JsonDecoder

  /** Every relay's view reported; decoding fails until all three have. */
  final case class Reports(served: Report, opaque: Report, control: Report) derives JsonDecoder

  /** The host page's origin, the Served relay's, and an unrelated page's. */
  final case class Origins(host: Origin, relay: Origin, elsewhere: Origin)

  /** What the unrelated page saw once it had waited: whether the relay it framed ever spoke. */
  final case class Embedded(spoke: Boolean) derives JsonDecoder

  private def sha256(script: String): String =
    val digest = MessageDigest.getInstance("SHA-256").digest(script.getBytes(StandardCharsets.UTF_8))
    s"'sha256-${Base64.getEncoder.encodeToString(digest)}'"

  /** The view's probe. It records CSP violations, runs its tries, and reports once they have settled. */
  private def probe(host: Origin): String =
    s"""const violations = [];
       |document.addEventListener("securitypolicyviolation", e => violations.push(e.effectiveDirective || e.violatedDirective));
       |const report = {};
       |report.origin = String(self.origin);
       |try { report.topLocation = String(window.top.location.href); } catch (e) { report.topThrows = e.name; }
       |fetch("${host.render}/ping").then(() => { report.fetch = "Resolved"; }, () => { report.fetch = "Rejected"; })
       |  .finally(() => setTimeout(() => {
       |    report.unhashedRan = window.unhashedRan === true;
       |    report.violations = violations;
       |    parent.postMessage({ spike: report }, "*");
       |  }, 300));""".stripMargin

  private val unhashed = "window.unhashedRan = true;"

  private def view(host: Origin): String =
    s"""<!doctype html><html><head><script>${probe(host)}</script></head>
       |<body><script>$unhashed</script></body></html>""".stripMargin

  /** A JS string literal that can sit inside `<script>`: its `</` would otherwise end the element early. */
  private def script(literal: String): String = literal.replace("</", "<\\/")

  private def attribute(value: String): String = value.replace("&", "&amp;").replace("\"", "&quot;")

  /** The relay frames the view and forwards whatever it posts to the page above. */
  private def relayScript(host: Origin): String =
    s"""const f = document.createElement("iframe");
       |f.setAttribute("sandbox", "allow-scripts");
       |f.srcdoc = ${script(Json.Str(view(host)).toJson)};
       |window.addEventListener("message", e => { if (e.source === f.contentWindow) parent.postMessage(e.data, "*"); });
       |document.body.appendChild(f);""".stripMargin

  /** Only the relay's script and the view's probe may run, and nothing may connect. */
  private def policy(host: Origin): String =
    s"default-src 'none'; script-src ${sha256(relayScript(host))} ${sha256(probe(host))}; connect-src 'none'"

  /** The served relay's header also names the one page that may frame it; a `<meta>` cannot carry this. */
  private def servedPolicy(host: Origin): String = s"${policy(host)}; frame-ancestors ${host.render}"

  /** A page on a third origin that frames the served relay and records whether anything inside it spoke. */
  private def embedPage(relay: Origin): String =
    s"""<!doctype html><html><head><title>elsewhere</title></head><body>
       |<script>
       |window.embedded = null;
       |let spoke = false;
       |window.addEventListener("message", e => { if (e.data && e.data.spike) spoke = true; });
       |setTimeout(() => { window.embedded = { spoke }; }, 3000);
       |</script>
       |<iframe sandbox="allow-scripts allow-same-origin" src="${relay.render}/relay"></iframe>
       |</body></html>""".stripMargin

  private def relayHtml(host: Origin, csp: Csp): String =
    val meta = csp match
      case Csp.Meta                => s"""<meta http-equiv="Content-Security-Policy" content="${policy(host)}">"""
      case Csp.Header | Csp.Absent => ""
    s"""<!doctype html><html><head>$meta</head><body><script>${relayScript(host)}</script></body></html>"""

  private def hostPage(origins: Origins): String =
    val srcdoc = (csp: Csp) => attribute(relayHtml(origins.host, csp))
    s"""<!doctype html><html><head><title>spike</title></head><body>
       |<script>
       |window.results = {};
       |const frames = ["served", "opaque", "control"];
       |window.addEventListener("message", e => {
       |  const which = frames.find(id => document.getElementById(id).contentWindow === e.source);
       |  if (which && e.data && e.data.spike) window.results[which] = e.data.spike;
       |});
       |</script>
       |<iframe id="served" sandbox="allow-scripts allow-same-origin" src="${origins.relay.render}/relay"></iframe>
       |<iframe id="opaque" sandbox="allow-scripts" srcdoc="${srcdoc(Csp.Meta)}"></iframe>
       |<iframe id="control" sandbox="allow-scripts" srcdoc="${srcdoc(Csp.Absent)}"></iframe>
       |</body></html>""".stripMargin
  end hostPage

  private val loopback = Server.Config.default.copy(host = "127.0.0.1", port = 0)

  private def at(server: Server): IO[OriginError, Origin] =
    server.port.flatMap(p => ZIO.fromEither(Origin.from(s"http://127.0.0.1:$p")))

  /** Two listeners, so the Served relay is a different origin from the host page. */
  private val servers: ZIO[Scope, ServerError | OriginError, Origins] =
    for
      bound <- Promise.make[Nothing, Origins]
      host  <- Server.install(
        Routes(
          Method.GET / "host" -> handler(bound.await.map(o => Response.html(hostPage(o)))),
          Method.GET / "ping" -> Handler.succeed(Response.text("pong").withHeader("Access-Control-Allow-Origin", "*")),
        ),
        loopback,
      )
      hostOrigin <- at(host)
      relay      <- Server.install(
        Routes(
          Method.GET / "relay" -> Handler.succeed(
            Response
              .html(relayHtml(hostOrigin, Csp.Header))
              .withHeader("Content-Security-Policy", servedPolicy(hostOrigin))
          )
        ),
        loopback,
      )
      relayOrigin <- at(relay)
      elsewhere   <- Server.install(
        Routes(Method.GET / "elsewhere" -> Handler.succeed(Response.html(embedPage(relayOrigin)))),
        loopback,
      )
      origins <- at(elsewhere).map(Origins(hostOrigin, relayOrigin, _))
      _       <- bound.succeed(origins)
    yield origins

  /** `evaluate` returns Playwright's serialized value, where a string is `{"s": "..."}`. */
  private def unwrap(raw: String): Either[String, String] =
    raw.fromJson[Json].flatMap {
      case Json.Obj(fields) => fields.collectFirst { case ("s", Json.Str(text)) => text }.toRight(s"not a string: $raw")
      case other            => Left(s"not a serialized value: $other")
    }

  /** `window.<name>`, polled until it decodes, or the last reading when it never does. */
  private def poll[A: JsonDecoder](page: Page, name: String): IO[ChekhovError | String, A] =
    val read = page
      .evaluate(s"() => JSON.stringify(window.$name)", isFunction = true)
      .map(raw => unwrap(raw).flatMap(text => text.fromJson[A].left.map(e => s"$e in $text")))
    read
      .repeat(Schedule.spaced(100.millis) *> Schedule.recurUntil(_.isRight))
      .timeout(15.seconds)
      .someOrElseZIO(read)
      .flatMap(r => ZIO.fromEither(r).mapError(e => s"window.$name never settled: $e"))

  private def sandboxed(r: Report): TestResult =
    assertTrue(r.origin == "null", r.topThrows.contains("SecurityError"), r.topLocation.isEmpty)

  private def confined(r: Report): TestResult =
    assertTrue(
      r.fetch == Fetch.Rejected,
      !r.unhashedRan,
      r.violations.contains("connect-src"),
      r.violations.exists(_.startsWith("script-src")),
    )

  private def unconfined(r: Report): TestResult =
    assertTrue(r.fetch == Fetch.Resolved, r.unhashedRan, r.violations.isEmpty)

  /** One test per browser, each with its own Playwright process. */
  private def on(browser: ChekhovBrowser) =
    test(
      s"${browser.channelName}: the view inherits its relay's CSP in both modes and cannot reach the top page, " +
        "and no other page can frame the served relay"
    ):
      val config =
        ChekhovConfig(browser = browser, headless = true, artifactsDir = java.nio.file.Path.of("target/chekhov"))
      ZIO
        .scoped:
          for
            origins  <- servers
            page     <- ZIO.service[Page]
            _        <- page.goto(s"${origins.host.render}/host")
            seen     <- poll[Reports](page, "results")
            _        <- page.goto(s"${origins.elsewhere.render}/elsewhere")
            embedded <- poll[Embedded](page, "embedded")
            // The per-browser record the design doc cites; CI logs keep it for each run.
            _ <- ZIO.logInfo(s"${browser.channelName} sandbox reports: $seen; framed elsewhere: $embedded")
          yield (sandboxed(seen.served) && confined(seen.served)).label("served") &&
            (sandboxed(seen.opaque) && confined(seen.opaque)).label("opaque") &&
            (sandboxed(seen.control) && unconfined(seen.control)).label("control") &&
            assertTrue(!embedded.spoke).label("framed elsewhere")
        .provide(ZLayer.succeed(config), PlaywrightDriver.suiteLayers)

  def spec = suite("MCP Apps sandbox: CSP inheritance (S2)")(
    on(ChekhovBrowser.Chromium),
    on(ChekhovBrowser.Firefox),
    on(ChekhovBrowser.WebKit),
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(120.seconds)
end CspInheritanceSpec
