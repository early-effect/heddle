package heddle.apps.browser.host

import heddle.Client
import heddle.mcp.apps.{HostPolicy, Origin}
import heddle.mcp.apps.frame.{Frame, RelayMode}
import heddle.mcp.apps.host.*
import heddle.mcp.apps.ui.*
import heddle.mcp.client.McpClient
import heddle.mcp.protocol.{Implementation, ToolName}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import zio.*
import zio.json.*
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
        .fromOption(BrowserDocument.elementById(PageConfig.ElementId))
        .orElseFail("no host config")
        .flatMap(e => ZIO.fromEither(e.textContent.fromJson[PageConfig]))
      _    <- ZIO.succeed { Published.heddleAudit = js.Array(); Published.heddleMounts = js.Array() }
      _    <- ZIO.serviceWithZIO[Audit](_.events.foreach(e => publish(Published.heddleAudit, e.toJson)).forkScoped)
      host <- AppsHost.make(settings)
      mode = config.relay.fold(RelayMode.Opaque)(RelayMode.Served(_))
      _ <- ZIO.foreachParDiscard(config.launches)(launch => serve(host, config.host, mode, launch))
    yield ()

  /** A Launch button and the frame under it, for one server. */
  private def serve(host: AppsHost, origin: Origin, mode: RelayMode, launch: Launch) =
    for
      session <- McpClient.http(launch.url, McpClient.Settings(Implementation("heddle-reference-host", "0")))
      server = AppServer(launch.server, session)
      button = BrowserDocument.createElement("button")
      slot   = BrowserDocument.createElement("div")
      _ <- ZIO.succeed {
        button.setAttribute("id", s"launch-${launch.server.value}")
        button.textContent = s"Launch ${launch.tool.value} on ${launch.server.value}"
        slot.setAttribute("id", s"view-${launch.server.value}")
        val _ = BrowserDocument.body.appendChild(button)
        val _ = BrowserDocument.body.appendChild(slot)
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
  def ask(request: ConsentRequest): UIO[ConsentOutcome] =
    ZIO.scoped(
      for
        _      <- ZIO.acquireRelease(ZIO.succeed(render(request)))(b => ZIO.succeed(b.remove()))
        choice <- ZStream
          .mergeAllUnbounded()(
            answers.map((id, outcome) => BrowserDocument.elementById(id).fold(ZStream.empty)(clicks(_).as(outcome)))*
          )
          .runHead
      yield choice.getOrElse(ConsentOutcome.Cancelled)
    )

  private val answers = List(
    "consent-allow-once"    -> ConsentOutcome.AllowOnce,
    "consent-allow-session" -> ConsentOutcome.AllowForSession,
    "consent-reject"        -> ConsentOutcome.Rejected,
  )

  private def render(request: ConsentRequest): Element =
    val box = BrowserDocument.createElement("div")
    box.setAttribute("id", "consent")
    val text = BrowserDocument.createElement("p")
    text.textContent = s"${request.server.value} wants to call ${request.tool.value} ${request.arguments.toJson}"
    val _ = box.appendChild(text)
    answers.foreach { (id, outcome) =>
      val b = BrowserDocument.createElement("button")
      b.setAttribute("id", id)
      b.textContent = outcome.toString
      val _ = box.appendChild(b)
    }
    val _ = BrowserDocument.body.appendChild(box)
    box
  end render

  /** One element per click; the listener comes off when the stream ends. */
  def clicks(target: Element): ZStream[Any, Nothing, Unit] =
    ZStream.asyncScoped[Any, Nothing, Unit] { emit =>
      ZIO.acquireRelease(ZIO.succeed {
        val listener: js.Function1[js.Any, Unit] = _ =>
          val _ = emit(ZIO.succeed(Chunk.unit))
        target.addEventListener("click", listener)
        listener
      })(listener => ZIO.succeed(target.removeEventListener("click", listener)))
    }
end Dialog
