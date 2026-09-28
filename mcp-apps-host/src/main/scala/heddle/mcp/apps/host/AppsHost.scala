package heddle.mcp.apps.host

import heddle.http.{Scheme, Url}
import heddle.mcp.apps.{Clamp, HostPolicy, UiPolicy}
import heddle.mcp.apps.ui.*
import heddle.mcp.client.McpError
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

/** How tall a host lets a view grow, in CSS pixels. */
final case class HeightBounds(min: Double, max: Double):
  def clamp(height: Double): Double = height.max(min).min(max)

object HeightBounds:
  val default: HeightBounds = HeightBounds(96, 720)

/** How many of a view's log lines reach the audit: `lines` every `per`, as a token bucket. */
final case class LogBudget(lines: Long, per: Duration):
  /** One token per element; an element with no token left is dropped, not delayed. */
  def enforce[A](stream: ZStream[Any, Nothing, A]): ZStream[Any, Nothing, A] =
    stream.rechunk(1).throttleEnforce(lines, per)(_.size.toLong)

object LogBudget:
  val default: LogBudget = LogBudget(60, 1.minute)

/** What a host is and allows. The same for every view it mounts. */
final case class HostSettings(
    info: Implementation,
    policy: HostPolicy,
    context: HostContext = HostContext.empty,
    modes: Chunk[DisplayMode] = Chunk(DisplayMode.Inline),
    heights: HeightBounds = HeightBounds.default,
    logs: LogBudget = LogBudget.default,
    teardownWait: Duration = 2.seconds,
)

/** What the host page does for one mounted view. */
trait ViewFrame:
  def resize(height: Double): UIO[Unit]
  def openLink(url: Url): UIO[Unit]

/** Why a mounted view stopped. */
enum Ending derives JsonCodec:
  case PortClosed
  case Navigated
  case TornDown(reason: String)

/** Mounts the views a host's servers link to the tools the model called. */
final class AppsHost private (settings: HostSettings, generations: Ref[Long]):
  /** Reads the linked view from the live server, pins its bytes, and clamps what it asks by what the host allows. Fails
    * closed: a refusal frames nothing.
    */
  def mount(server: AppServer, launched: Launched): ZIO[HashPins & Audit, MountRefusal, Mount] =
    for
      generation <- generations.updateAndGet(_ + 1).map(Generation(_))
      live       <- server.session.listTools.mapError(MountRefusal.Session(_))
      view       <- ZIO.fromEither(LinkedView.of(server.name, launched.tool, live))
      contents   <- server.session.readResource(view.uri.value).mapError(MountRefusal.Session(_))
      resource   <- ZIO.fromEither(ViewResource.of(view.uri, contents))
      record = (decision: Decision) =>
        Clock.instant.flatMap(at =>
          ZIO.serviceWithZIO[Audit](_.record(AuditEvent(at, server.name, view.uri, generation, Action.Mount, decision)))
        )
      pin <- ZIO.serviceWithZIO[HashPins](_.check(PinKey(server.name, view.uri), resource.digest))
      _   <- pin match
        case PinCheck.Changed(pinned) =>
          record(Decision.HashMismatch(pinned, resource.digest)) *>
            ZIO.fail(MountRefusal.HashMismatch(view.uri, pinned, resource.digest))
        case PinCheck.FirstSeen | PinCheck.Same => ZIO.unit
      clamp     = Clamp[UiPolicy, HostPolicy]
      effective = clamp.clamp(resource.ask, settings.policy)
      _ <- ZIO.foreachDiscard(clamp.narrowed(resource.ask, settings.policy))(n => record(Decision.Narrowed(n)))
      _ <- ZIO.foreachDiscard(resource.problems)(p => record(Decision.Dropped(p)))
      _ <- record(Decision.Allowed)
    yield Mount(
      settings,
      server,
      view,
      launched,
      resource.html,
      effective,
      resource.scripts,
      generation,
    )
end AppsHost

object AppsHost:
  def make(settings: HostSettings): UIO[AppsHost] = Ref.make(0L).map(AppsHost(settings, _))

/** One view, read, pinned, and clamped, ready to frame. `serve` is the host's side of the view protocol. */
final case class Mount(
    settings: HostSettings,
    server: AppServer,
    view: LinkedView,
    launched: Launched,
    html: String,
    policy: UiPolicy,
    scripts: Csp.Scripts,
    generation: Generation,
):
  /** What the relay gives the view: the clamped network and permissions. */
  def grant: SandboxGrant = SandboxGrant(policy.network, policy.permissions)

  /** Answers the view over `port` until it ends: the relay reports a navigation, the view asks to go, or the port
    * closes. `teardown` on the result asks the view to go.
    */
  def serve(port: ViewPort, frame: ViewFrame): URIO[Scope & ConsentGate & Audit, Mounted] =
    for
      gate    <- ZIO.service[ConsentGate]
      audit   <- ZIO.service[Audit]
      state   <- Ref.make(Mount.State())
      ended   <- Promise.make[Nothing, Ending]
      leaving <- Promise.make[Nothing, String]
      pending <- Ref.make(Map.empty[RequestId, Promise[Nothing, Unit]])
      // Bounded, so a view that floods the log costs the host at most one budget of memory.
      logs <- Queue.dropping[LoggingLevel](settings.logs.lines.toInt.max(1))
      talk = Mount.Conversation(this, port, frame, gate, audit, state, ended, leaving, pending, logs)
      _ <- talk.drainLogs.forkScoped
      _ <- port.receive
        .interruptWhen(ended.await)
        .foreach(talk.handle)
        .ignore
        .zipRight(ended.succeed(Ending.PortClosed))
        .forkScoped
      // Apart from the receive loop, which is what hears the view's answer to the teardown request.
      _ <- leaving.await.flatMap(talk.leave).forkScoped
    yield Mounted(leaving, ended)
end Mount

/** A view being served. */
final class Mounted private[host] (leaving: Promise[Nothing, String], ended: Promise[Nothing, Ending]):
  /** Why it stopped, once it has. */
  def ending: UIO[Ending] = ended.await

  /** Asks the view to save what it must and go, waits for its answer or the host's patience, then ends it. */
  def teardown(reason: String): UIO[Ending] = leaving.succeed(reason) *> ended.await

object Mount:
  final case class State(
      greeted: Boolean = false,
      initialized: Boolean = false,
      viewModes: Chunk[DisplayMode] = Chunk.empty,
      mode: DisplayMode = DisplayMode.Inline,
  )

  private def json[A: JsonEncoder](a: A): Json.Obj =
    a.toJsonAST.toOption.collect { case o: Json.Obj => o }.getOrElse(Json.Obj())

  /** A refusal a view reads as a failed request: the spec's implementation-defined code, and the reason. */
  private def refused(denial: Denial): RpcError = RpcError.Other(-32000, denial.message, None)

  private[host] final case class Conversation(
      mount: Mount,
      port: ViewPort,
      frame: ViewFrame,
      gate: ConsentGate,
      audit: Audit,
      state: Ref[State],
      ended: Promise[Nothing, Ending],
      leaving: Promise[Nothing, String],
      pending: Ref[Map[RequestId, Promise[Nothing, Unit]]],
      logs: Queue[LoggingLevel],
  ):
    private val settings = mount.settings
    private val view     = mount.view

    private def record(action: Action, decision: Decision): UIO[Unit] =
      Clock.instant.flatMap(at =>
        audit.record(AuditEvent(at, view.server, view.uri, mount.generation, action, decision))
      )

    private def send(message: Message): UIO[Unit] =
      port.send(message).catchAll(_ => ended.succeed(Ending.PortClosed).unit)

    private def answer(id: RequestId, result: Json.Obj): UIO[Unit] = send(Message.Result(id, result))
    private def fail(id: RequestId, error: RpcError): UIO[Unit]    = send(Message.Error(Some(id), error))

    /** Nothing is handled once the mount has ended: the stream's interruption lands asynchronously, and a message
      * queued behind a navigation is not from the view the host framed.
      */
    def handle(message: Message): UIO[Unit] =
      ZIO.unlessZIODiscard(ended.isDone)(message match
        case Message.Request(id, method, params)  => request(id, method, params)
        case Message.Notification(method, params) => notification(method, params)
        case Message.Result(id, _)                => settle(id)
        case Message.Error(Some(id), _)           => settle(id)
        case Message.Error(None, _)               => ZIO.unit)

    private def settle(id: RequestId): UIO[Unit] =
      pending.modify(p => (p.get(id), p - id)).flatMap(ZIO.foreachDiscard(_)(_.succeed(())))

    private def request(id: RequestId, method: String, params: Json.Obj): UIO[Unit] =
      ViewRequest.decode(method, params) match
        case Left(UiError.UnknownMethod(m)) =>
          record(Action.Request(m, None, None), Decision.Denied(Denial.NotOffered(m))) *>
            fail(id, RpcError.methodNotFound(m))
        case Left(e) =>
          record(Action.Request(method, None, None), Decision.Denied(Denial.Malformed(e.message))) *>
            fail(id, RpcError.InvalidParams(e.message))
        case Right(req) => viewRequest(id, req)

    private def viewRequest(id: RequestId, req: ViewRequest): UIO[Unit] =
      val plain = Action.Request(req.method, None, None)
      req match
        case ViewRequest.Initialize(_, caps, _) =>
          // A document says ui/initialize once. A second one is a new document in the view's frame (a reload, or a
          // navigation whose script ran before the relay saw the frame load), so the mount ends as a navigation.
          state
            .modify(s =>
              (s.greeted, s.copy(greeted = true, viewModes = caps.availableDisplayModes.getOrElse(Chunk.empty)))
            )
            .flatMap { again =>
              if again then
                record(Action.Navigated, Decision.DroppedAfterNavigate) *> ended.succeed(Ending.Navigated).unit
              else record(plain, Decision.Allowed) *> answer(id, json(initializeResult))
            }
        case ViewRequest.CallTool(name, arguments) => callTool(id, name, arguments)
        case ViewRequest.ReadResource(uri)         =>
          if uri != view.uri.value then
            record(plain, Decision.Denied(Denial.NotTheView(uri))) *> fail(id, refused(Denial.NotTheView(uri)))
          else
            record(plain, Decision.Allowed) *>
              mount.server.session
                .readResource(uri)
                .foldZIO(e => fail(id, RpcError.Internal(e.message)), c => answer(id, json(ReadResourceResult(c))))
        case ViewRequest.OpenLink(url) =>
          Url.decode(url).toOption.filter(link) match
            case Some(u) =>
              record(plain, Decision.Allowed) *> frame.openLink(u) *> answer(id, Outcome.wire(Outcome.Done))
            case None =>
              record(plain, Decision.Denied(Denial.NotALink(url))) *> answer(id, Outcome.wire(Outcome.Refused))
        case ViewRequest.RequestDisplayMode(asked) =>
          state
            .modify { s =>
              val ok   = settings.modes.contains(asked) && (s.viewModes.isEmpty || s.viewModes.contains(asked))
              val mode = if ok then asked else s.mode
              (mode, s.copy(mode = mode))
            }
            .flatMap(mode => record(plain, Decision.Allowed) *> answer(id, Json.Obj("mode" -> Json.Str(mode.wire))))
        case ViewRequest.SendMessage(_) | ViewRequest.DownloadFile(_) =>
          record(plain, Decision.Denied(Denial.NotOffered(req.method))) *> answer(id, Outcome.wire(Outcome.Refused))
        case ViewRequest.UpdateModelContext(_, _) =>
          record(plain, Decision.Denied(Denial.NotOffered(req.method))) *>
            fail(id, refused(Denial.NotOffered(req.method)))
        case ViewRequest.Ping => answer(id, Json.Obj())
      end match
    end viewRequest

    private def link(u: Url): Boolean = u.absolute && u.scheme.exists(s => s == Scheme.Http || s == Scheme.Https)

    /** The shed gate, then the one consent gate, then the server. Nothing reaches the server that both did not pass. */
    private def callTool(id: RequestId, name: ToolName, arguments: Json.Obj): UIO[Unit] =
      val action = Action.Request("tools/call", Some(name), Some(Digest.of(arguments.toJson)))
      if !view.admits(name) then
        record(action, Decision.Denied(Denial.NotLinked(name))) *> fail(id, refused(Denial.NotLinked(name)))
      else
        gate.decide(ConsentRequest(view.server, view.uri, name, arguments)).flatMap { outcome =>
          record(action, Decision.ConsentAsked(outcome)) *> {
            if !outcome.allows then
              record(action, Decision.Denied(Denial.ConsentRefused(outcome))) *>
                fail(id, refused(Denial.ConsentRefused(outcome)))
            else
              record(action, Decision.Allowed) *>
                call(Cleared(name, arguments)).foldZIO(
                  e => fail(id, RpcError.Internal(e.message)),
                  result => answer(id, json(result)),
                )
          }
        }
      end if
    end callTool

    private def call(cleared: Cleared): IO[McpError, CallToolResult] =
      mount.server.session.callTool(cleared.tool, cleared.arguments)

    private def notification(method: String, params: Json.Obj): UIO[Unit] =
      if method.startsWith(SandboxMessage.Prefix) then sandbox(method, params)
      else
        ViewNotification.decode(method, params) match
          case Right(ViewNotification.Initialized) =>
            state
              .modify(s => (s.initialized, s.copy(initialized = true)))
              .flatMap { already =>
                ZIO.unless(already)(
                  record(Action.Notification(method), Decision.Allowed) *>
                    send(HostNotification.ToolInput(mount.launched.input).message) *>
                    send(HostNotification.ToolResult(mount.launched.result).message)
                )
              }
              .unit
          case Right(ViewNotification.SizeChanged(_, height)) =>
            ZIO.foreach(height)(h => frame.resize(settings.heights.clamp(h))).unit
          case Right(ViewNotification.RequestTeardown)  => leaving.succeed("the view asked to close").unit
          case Right(ViewNotification.Log(level, _, _)) => log(level)
          case Left(_)                                  => ZIO.unit

    /** Only the relay speaks these; it never forwards one from the view. */
    private def sandbox(method: String, params: Json.Obj): UIO[Unit] =
      SandboxMessage.decode(method, params) match
        case Right(SandboxMessage.Navigated) =>
          record(Action.Navigated, Decision.DroppedAfterNavigate) *> ended.succeed(Ending.Navigated).unit
        case _ => ZIO.unit

    /** At most `logsPerMinute` log lines reach the audit in any minute-long window; the rest are dropped. */
    private def log(level: LoggingLevel): UIO[Unit] = logs.offer(level).unit

    /** The audit's side of `logs`, within `settings.logs`. */
    val drainLogs: UIO[Unit] =
      settings.logs
        .enforce(ZStream.fromQueue(logs))
        .foreach(level => record(Action.Notification("notifications/message"), Decision.Logged(level)))

    /** One teardown per mount: the first reason wins, and a mount that already ended is left alone. */
    def leave(reason: String): UIO[Unit] =
      val id = RequestId.Str(s"heddle-teardown-${mount.generation.value}")
      ZIO.unlessZIODiscard(ended.isDone)(
        for
          heard <- Promise.make[Nothing, Unit]
          _     <- pending.update(_.updated(id, heard))
          _     <- send(HostRequest.ResourceTeardown(Some(reason)).message(id))
          _     <- heard.await.timeout(settings.teardownWait)
          _     <- record(Action.Teardown, Decision.TornDown(reason))
          _     <- ended.succeed(Ending.TornDown(reason))
        yield ()
      )
    end leave

    private def initializeResult: InitializeResult =
      InitializeResult(
        UiProtocol.Version,
        settings.info,
        HostCapabilities(
          openLinks = Some(Present),
          serverTools = Some(ListChanged()),
          serverResources = Some(ListChanged()),
          logging = Some(Present),
          sandbox = Some(mount.grant),
        ),
        settings.context.copy(
          toolInfo = Some(ToolInfo(None, view.launch)),
          displayMode = Some(DisplayMode.Inline),
          availableDisplayModes = Some(settings.modes),
        ),
      )
  end Conversation

  /** Proof that the shed gate and the consent gate both passed. Only `callTool` makes one, and only a `Cleared` call
    * reaches the server.
    */
  private final case class Cleared(tool: ToolName, arguments: Json.Obj)
end Mount
