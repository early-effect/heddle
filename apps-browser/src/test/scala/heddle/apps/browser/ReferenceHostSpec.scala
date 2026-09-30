package heddle.apps.browser

import chekhov.*
import chekhov.driver.PlaywrightDriver
import heddle.*
import heddle.apps.browser.counter.{Count, Counter}
import heddle.apps.browser.host.{Launch, MountOutcome, PageConfig}
import heddle.error.ServerError
import heddle.http.MediaType
import heddle.mcp.{Mcp, McpBuildError}
import heddle.mcp.apps.{AppBuildError, Origin, OriginError, UiDocument, withApp}
import heddle.mcp.apps.host.*
import heddle.mcp.protocol.ToolName
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** The reference host in real browsers, through both relay modes: a heddle view that calls a tool with consent, a
  * hostile view that tries everything a view must not, and a view whose bytes change.
  */
object ReferenceHostSpec extends ZIOSpecDefault:
  /** Where the suite's two listeners are, and what it can reach behind the pages. */
  final case class Site(host: Origin, relay: Origin, count: Ref[Int], bashed: Ref[Int], changed: Ref[Boolean])

  type SetupError = ServerError | OriginError | NonEmptyChunk[McpBuildError] | NonEmptyChunk[AppBuildError]

  private val counter = ServerName("counter")
  private val hostile = ServerName("hostile")

  private def resource(name: String): IO[String, String] =
    ZIO
      .attemptBlocking(scala.io.Source.fromResource(s"apps-browser/$name", getClass.getClassLoader).mkString)
      .mapError(e => s"$name is not on the test classpath: ${e.getMessage}")

  private def script(js: String): Response = Response(Status.Ok).withBody(Body.text(js, MediaType.JavascriptUtf8))

  /** `window.<name>` decoded as `A`, polled until `ok` holds, or the last reading when it never does. */
  private def poll[A: JsonDecoder](page: Page, name: String)(ok: A => Boolean): IO[ChekhovError | String, A] =
    val read = page
      .evaluate(s"() => JSON.stringify(window.$name ?? null)", isFunction = true)
      .map(raw =>
        raw
          .fromJson[Json]
          .flatMap {
            case Json.Obj(fields) => fields.collectFirst { case ("s", Json.Str(t)) => t }.toRight(s"not a string: $raw")
            case other            => Left(s"not a serialized value: $other")
          }
          .flatMap(_.fromJson[A])
      )
    read
      .repeat(Schedule.spaced(100.millis) *> Schedule.recurUntil(_.exists(ok)))
      .timeout(20.seconds)
      .someOrElseZIO(read)
      .flatMap(r => ZIO.fromEither(r).filterOrElseWith(ok)(a => ZIO.fail(s"window.$name never settled: $a")))
  end poll

  /** A selector inside the view: past the relay's iframe, then the view's. */
  private def inView(server: ServerName, selector: String): String =
    s"#view-${server.value} iframe >> internal:control=enter-frame >> iframe >> internal:control=enter-frame >> $selector"

  private def text(page: Page, selector: String)(ok: String => Boolean): IO[ChekhovError | String, String] =
    page
      .textContent(selector)
      .either
      .repeat(Schedule.spaced(100.millis) *> Schedule.recurUntil(_.exists(ok)))
      .timeout(20.seconds)
      .flatMap {
        case Some(Right(t)) if ok(t) => ZIO.succeed(t)
        case other                   => ZIO.fail(s"$selector never read as expected: $other")
      }

  private def calls(log: Chunk[AuditEvent], server: ServerName): Chunk[(Option[ToolName], Decision)] =
    log.collect { case AuditEvent(_, `server`, _, _, Action.Request("tools/call", tool, _), d) =>
      (tool, d)
    }

  /** The host page, the counter and hostile MCP servers behind it, and the relay listener beside it. */
  private val site: ZIO[Scope, SetupError | String, Site] =
    for
      view    <- resource("counter-view.js")
      page    <- resource("reference-host.js")
      count   <- Ref.make(0)
      bashed  <- Ref.make(0)
      changed <- Ref.make(false)
      bound   <- Promise.make[Nothing, (Origin, Option[Origin])]
      counterApi = Api("Counter", "1.0.0")
        .job(Counter.show)(_ => count.get.map(Count(_)))
        .resource(Counter.inc)(_ => count.updateAndGet(_ + 1).map(Count(_)))
      original <- ZIO.fromEither(Mcp.from(counterApi).map(_.at("mcp/counter")))
      pinned   <- ZIO.fromEither(original.withApp(Counter.shed, UiDocument("Counter", view)))
      altered  <- ZIO.fromEither(original.withApp(Counter.shed, UiDocument("Counter", view + "\n// changed")))
      hostileApi = Api("Hostile", "1.0.0")
        .job(Hostile.show)(_ => ZIO.succeed("hostile"))
        .job(Hostile.bash)(_ => bashed.update(_ + 1).as("ran"))
      hostileMcp <- ZIO.fromEither(Mcp.from(hostileApi).map(_.at("mcp/hostile")))
      hostileApp <- ZIO.fromEither(hostileMcp.withApp(Hostile.shed, Hostile.document))
      // The counter's bytes, as the test chooses them: the pinned view, or one byte-different.
      counterRoutes = Routes.fromHandler(
        Handler.fromFunctionZIO(req => changed.get.flatMap(c => (if c then altered else pinned).routes(req)))
      )
      pageRoutes = Routes(
        Method.GET / "host" -> handler { req =>
          bound.await.map { (h, relay) =>
            val served = req.query.get("mode").contains("served")
            val config = PageConfig(
              h,
              relay.filter(_ => served),
              Chunk(
                Launch(counter, s"${h.render}/mcp/counter", ToolName("show_counter")),
                Launch(hostile, s"${h.render}/mcp/hostile", ToolName("show_hostile")),
              ),
            )
            Response.html(
              s"""<!doctype html><html><head><meta charset="utf-8"><title>reference host</title></head><body>""" +
                s"""<script type="application/json" id="${PageConfig.ElementId}">${config.toJson
                    .replace("</", "<\\/")}</script>""" +
                """<script src="/reference-host.js"></script></body></html>"""
            )
          }
        },
        Method.GET / "reference-host.js" -> Handler.succeed(script(page)),
      )
      loopback = Server.Config.default.copy(host = "127.0.0.1", port = 0)
      hostServer  <- Server.install(pageRoutes ++ counterRoutes ++ hostileApp.routes, loopback)
      host        <- hostServer.port.flatMap(p => ZIO.fromEither(Origin.from(s"http://127.0.0.1:$p")))
      relayServer <- Server.install(RelayRoutes(host), loopback)
      relay       <- relayServer.port.flatMap(p => ZIO.fromEither(Origin.from(s"http://127.0.0.1:$p")))
      _           <- bound.succeed((host, Some(relay)))
    yield Site(host, relay, count, bashed, changed)

  /** The counter: a click asks the user, and only after they allow it does `inc` reach the server, once. */
  private def consented(page: Page, s: Site) =
    for
      _     <- page.click(s"#launch-${counter.value}")
      _     <- text(page, inView(counter, "#count"))(_ == "0")
      _     <- page.click(inView(counter, "#inc"))
      _     <- page.click("#consent-allow-once")
      shown <- text(page, inView(counter, "#count"))(_ == "1")
      log   <- poll[Chunk[AuditEvent]](page, "heddleAudit")(l => calls(l, counter).size >= 2)
      n     <- s.count.get
    yield assertTrue(
      shown == "1",
      n == 1,
      calls(log, counter) ==
        Chunk(
          Some(ToolName("inc")) -> Decision.ConsentAsked(ConsentOutcome.AllowOnce),
          Some(ToolName("inc")) -> Decision.Allowed,
        ),
    )

  /** The hostile view: every call is refused, the CSP holds, the relay's own message cannot be spoofed, and a reload
    * ends the mount.
    */
  private def refused(page: Page, s: Site) =
    for
      _        <- page.click(s"#launch-${hostile.value}")
      raw      <- text(page, inView(hostile, "#findings"))(_.nonEmpty)
      findings <- ZIO.fromEither(raw.fromJson[Hostile.Findings])
      _        <- page.click(inView(hostile, "#reload"))
      _        <- poll[Chunk[MountOutcome]](page, "heddleMounts")(_.exists {
        case MountOutcome.Ended(`hostile`, Ending.Navigated) => true
        case _                                               => false
      })
      log    <- poll[Chunk[AuditEvent]](page, "heddleAudit")(_.exists(_.decision == Decision.DroppedAfterNavigate))
      bashes <- s.bashed.get
      n      <- s.count.get
      isError = (j: Json) => j.toJson.contains("\"error\"")
    yield assertTrue(
      isError(findings.bash),
      isError(findings.crossServer),
      isError(findings.context),
      findings.pingAfterSpoof.toJson.contains("\"result\""),
      findings.fetch == "rejected",
      findings.violations.contains("connect-src"),
      calls(log, hostile).map(_._2) == Chunk(
        Decision.Denied(Denial.NotLinked(ToolName("bash"))),
        Decision.Denied(Denial.NotLinked(ToolName("inc"))),
      ),
      log.exists(e =>
        e.server == hostile && e.decision == Decision.Denied(Denial.NotOffered("ui/update-model-context"))
      ),
      log.exists(e => e.server == hostile && e.decision.isInstanceOf[Decision.Narrowed]),
      log.exists(e => e.server == hostile && e.decision == Decision.DroppedAfterNavigate),
      bashes == 0,
      n == 1,
    )

  /** Changed bytes: the second launch is refused as a hash mismatch, and the first view stays the only one. */
  private def changedBytes(page: Page, s: Site) =
    for
      _      <- s.changed.set(true)
      _      <- page.click(s"#launch-${counter.value}")
      mounts <- poll[Chunk[MountOutcome]](page, "heddleMounts")(_.exists {
        case MountOutcome.Refused(`counter`, _) => true
        case _                                  => false
      })
      log    <- poll[Chunk[AuditEvent]](page, "heddleAudit")(_.exists(_.decision.isInstanceOf[Decision.HashMismatch]))
      frames <- page.evaluate(
        s"() => document.querySelectorAll('#view-${counter.value} iframe').length",
        isFunction = true,
      )
    yield assertTrue(
      log.exists(e => e.server == counter && e.decision.isInstanceOf[Decision.HashMismatch]),
      mounts.count { case MountOutcome.Mounted(`counter`, _) => true; case _ => false } == 1,
      frames.contains("1"),
    )

  private def on(browser: ChekhovBrowser, mode: String) =
    test(
      s"${browser.channelName}, $mode relay: consent gates the one call, a hostile view is refused, changed bytes are not mounted"
    ):
      val config =
        ChekhovConfig(browser = browser, headless = true, artifactsDir = java.nio.file.Path.of("target/chekhov"))
      ZIO
        .scoped:
          for
            s    <- site
            page <- ZIO.service[Page]
            _    <- page.goto(s"${s.host.render}/host?mode=$mode")
            a    <- consented(page, s)
            b    <- refused(page, s)
            c    <- changedBytes(page, s)
            log  <- poll[Chunk[AuditEvent]](page, "heddleAudit")(_.nonEmpty)
            _    <- ZIO.logInfo(
              s"${browser.channelName} $mode audit: ${log.map(e => s"${e.server.value} ${e.action} ${e.decision}").mkString("; ")}"
            )
          yield a.label("consent") && b.label("hostile") && c.label("changed bytes")
        .provide(ZLayer.succeed(config), PlaywrightDriver.suiteLayers)

  def spec = suite("Reference host in real browsers")(
    List(ChekhovBrowser.Chromium, ChekhovBrowser.Firefox, ChekhovBrowser.WebKit).flatMap(b =>
      List(on(b, "served"), on(b, "opaque"))
    )*
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(180.seconds)
end ReferenceHostSpec
