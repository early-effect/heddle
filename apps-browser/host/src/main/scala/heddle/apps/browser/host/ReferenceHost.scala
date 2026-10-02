package heddle.apps.browser.host

import ascent.dom
import heddle.Client
import heddle.mcp.apps.{HostPolicy, Origin}
import heddle.mcp.apps.frame.{Frame, RelayMode}
import heddle.mcp.apps.host.*
import heddle.mcp.client.McpClient
import heddle.mcp.protocol.{Implementation, ToolName}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

/** Where the page publishes what it saw, for a test or a person at the console. */
@js.native
@JSGlobal("window")
private object Published extends js.Object:
  var heddleAudit: js.Array[js.Any]  = js.native
  var heddleMounts: js.Array[js.Any] = js.native

/** The reference host page: the host kit, the frame, and the MCP client, all in the browser, as a real host runs them.
  * Each Launch button calls its tool as the model would, then mounts the view the tool links to.
  */
object ReferenceHost extends ZIOAppDefault:
  private val settings = HostSettings(Implementation("heddle-reference-host", "0"), HostPolicy.closed)

  def run =
    program
      .provideSome[Scope](HashPins.inMemory, Audit.layer(), ConsentGate.remembering(Dialog.ask), Client.live)
      .catchAll(e => ZIO.logError(s"reference host: $e"))

  private def program =
    for
      config <- ZIO
        .fromOption(dom.document.getElementById(PageConfig.ElementId).flatMap(_.textContent))
        .orElseFail("no host config")
        .flatMap(text => ZIO.fromEither(text.fromJson[PageConfig]))
      body <- ZIO.fromOption(dom.document.body).orElseFail("the page has no body")
      _    <- ZIO.succeed { Published.heddleAudit = js.Array(); Published.heddleMounts = js.Array() }
      log  <- ZIO.serviceWithZIO[Audit](_.events)
      _    <- log.foreach(e => publish(Published.heddleAudit, e.toJson)).forkScoped
      host <- AppsHost.make(settings)
      mode = config.relay.fold(RelayMode.Opaque)(RelayMode.Served(_))
      _ <- ZIO.foreachParDiscard(config.launches)(launch => serve(host, config.host, mode, body, launch))
    yield ()

  /** A Launch button and the frame under it, for one server. */
  private def serve(host: AppsHost, origin: Origin, mode: RelayMode, body: dom.HTMLElement, launch: Launch) =
    for
      session <- McpClient.http(launch.url, McpClient.Settings(Implementation("heddle-reference-host", "0")))
      server = AppServer(launch.server, session)
      (button, slot) <- ZIO.succeed {
        val button = dom.document.createElement(dom.HtmlTag.button)
        val slot   = dom.document.createElement(dom.HtmlTag.div)
        button.setAttribute("id", s"launch-${launch.server.value}")
        button.textContent = Some(s"Launch ${launch.tool.value} on ${launch.server.value}")
        slot.setAttribute("id", s"view-${launch.server.value}")
        val _ = body.appendChild(button)
        val _ = body.appendChild(slot)
        (button, slot)
      }
      _ <- Dialog.clicks(button).foreach { _ =>
        session
          .callTool(launch.tool, zio.json.ast.Json.Obj())
          .map(Launched(launch.tool, zio.json.ast.Json.Obj(), _))
          .mapError(e => MountRefusal.Session(e))
          .flatMap(host.mount(server, _))
          .foldZIO(
            refusal => outcome(MountOutcome.Refused(launch.server, refusal.message)),
            mount =>
              // Each mount lives in its own scope, so its frame goes when it ends.
              ZIO
                .scoped(
                  Frame.mount(mount, mode, slot, origin).flatMap { served =>
                    outcome(MountOutcome.Mounted(launch.server, mount.generation)) *>
                      served.ending
                        .flatMap(e => outcome(MountOutcome.Ended(launch.server, e)))
                  }
                )
                .catchAll(e => outcome(MountOutcome.Refused(launch.server, e.message)))
                // The mount belongs to the page: it lives in the page's scope, not past it.
                .forkScoped
                .unit,
          )
      }
    yield ()

  private def publish(to: js.Array[js.Any], json: String): UIO[Unit] = ZIO.succeed(to.push(js.JSON.parse(json))).unit

  private def outcome(o: MountOutcome): UIO[Unit] = publish(Published.heddleMounts, o.toJson)
end ReferenceHost

/** The host's consent dialog, on the host page's own origin: the one answerer a view can never reach. */
object Dialog:
  /** A page with no body cannot show the question, so nobody consented: `Cancelled`. */
  def ask(request: ConsentRequest): UIO[ConsentOutcome] =
    ZIO
      .scoped(
        ZIO.foreach(dom.document.body) { body =>
          for
            box    <- ZIO.acquireRelease(ZIO.succeed(render(request, body)))(b => ZIO.succeed(b.remove()))
            choice <- ZStream
              .mergeAllUnbounded()(
                answers.map((id, outcome) => box.querySelector(s"#$id").fold(ZStream.empty)(clicks(_).as(outcome)))*
              )
              .runHead
          yield choice
        }
      )
      .map(_.flatten.getOrElse(ConsentOutcome.Cancelled))

  private val answers = List(
    "consent-allow-once"    -> ConsentOutcome.AllowOnce,
    "consent-allow-session" -> ConsentOutcome.AllowForSession,
    "consent-reject"        -> ConsentOutcome.Rejected,
  )

  private def render(request: ConsentRequest, body: dom.HTMLElement): dom.HTMLDivElement =
    val box = dom.document.createElement(dom.HtmlTag.div)
    box.setAttribute("id", "consent")
    val text = dom.document.createElement(dom.HtmlTag.p)
    val action = request.summary.map(_.trim).filter(_.nonEmpty).getOrElse(request.tool.value)
    text.textContent = Some(s"$action on ${request.server.value}?")
    val _ = box.appendChild(text)
    request.arguments.fields.foreach { (name, value) =>
      val row = dom.document.createElement(dom.HtmlTag.p)
      val shown = value match
        case Json.Str(s)  => s
        case Json.Num(n)  => n.toString
        case Json.Bool(b) => b.toString
        case Json.Null    => ""
        case other        => other.toJson
      row.textContent = Some(s"$name: $shown")
      val _ = box.appendChild(row)
    }
    answers.foreach { (id, outcome) =>
      val b = dom.document.createElement(dom.HtmlTag.button)
      b.setAttribute("id", id)
      b.textContent = Some(outcome.toString)
      val _ = box.appendChild(b)
    }
    val _ = body.appendChild(box)
    box
  end render

  /** One element per click; the listener comes off when the stream ends. */
  def clicks(target: dom.EventTarget): ZStream[Any, Nothing, Unit] =
    ZStream.asyncScoped[Any, Nothing, Unit] { emit =>
      ZIO.acquireRelease(ZIO.succeed {
        val listener: js.Function1[dom.Event, Unit] = _ =>
          val _ = emit(ZIO.succeed(Chunk.unit))
        target.addEventListener("click", listener)
        listener
      })(listener => ZIO.succeed(target.removeEventListener("click", listener)))
    }
end Dialog
