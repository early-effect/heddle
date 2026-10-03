package heddle.mcp.apps.ui

import heddle.mcp.apps.{Grant, Shed}
import heddle.mcp.client.{McpError, McpSession}
import heddle.mcp.protocol.*
import scala.NamedTuple.NamedTuple
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{SubscriptionRef, ZStream}

/** A view's connection to its host. It calls only its shed's grants, by picking them (`bridge.call(_.inc)(())`), and
  * renders the launch tool's `Run` with the launch grant's own types.
  */
final class AppBridge[In, Err, Out, N <: Tuple, V <: Tuple] private (
    val shed: Shed[?, N, V],
    val host: InitializeResult,
    session: AppBridge.HostSession,
    runs: SubscriptionRef[Run[In, Err, Out]],
    contexts: SubscriptionRef[HostContext],
    updates: Hub[String],
):
  /** Pins a grant from the shed; `apply` sends its typed input and reads its typed output or declared error. */
  def call[I, E, O](pick: NamedTuple[N, V] => Grant[I, E, O]): McpSession.CallPartiallyApplied[I, E, O] =
    session.call(pick(shed.tools).endpoint)

  /** The launch tool's lifecycle, now and as the host reports it. */
  def run: ZStream[Any, Nothing, Run[In, Err, Out]] = runs.changes

  /** The host context from `ui/initialize`, with each `host-context-changed` patch merged in. */
  def context: ZStream[Any, Nothing, HostContext] = contexts.changes

  def openLink(url: String): IO[McpError, Outcome] =
    session.ask(ViewRequest.OpenLink(url)).map(Outcome.fromWire)

  def sendMessage(content: Chunk[ContentBlock]): IO[McpError, Outcome] =
    session.ask(ViewRequest.SendMessage(content)).map(Outcome.fromWire)

  def downloadFile(contents: Chunk[ContentBlock]): IO[McpError, Outcome] =
    session.ask(ViewRequest.DownloadFile(contents)).map(Outcome.fromWire)

  /** The mode the host actually chose, which may not be the one asked for. */
  def requestDisplayMode(mode: DisplayMode): IO[McpError, DisplayMode] =
    session.ask(ViewRequest.RequestDisplayMode(mode)).flatMap { r =>
      ZIO.fromEither(r.get("mode").toRight("no mode").flatMap(_.as[DisplayMode])).mapError(McpError.Protocol(_))
    }

  /** What the model sees of this view on its next turn; each update replaces the last. */
  def updateModelContext(content: Chunk[ContentBlock], structured: Option[Json.Obj] = None): IO[McpError, Unit] =
    session.ask(ViewRequest.UpdateModelContext(content, structured)).unit

  def sizeChanged(width: Option[Double], height: Option[Double]): IO[McpError, Unit] =
    session.tell(ViewNotification.SizeChanged(width, height))

  /** Asks the host to close this view. The host decides, and sends `ui/resource-teardown` if it agrees. */
  def requestTeardown: IO[McpError, Unit] = session.tell(ViewNotification.RequestTeardown)

  def log(level: LoggingLevel, data: Json, logger: Option[String] = None): IO[McpError, Unit] =
    session.tell(ViewNotification.Log(level, logger, data))

  /** The host advertised experimental `resources/subscribe`. A view that did not read this must not send it. */
  def offersResourceSubscribe: Boolean =
    host.hostCapabilities.experimental.exists {
      _.get("serverResources") match
        case Some(body: Json.Obj) => body.get("subscribe").contains(Json.Bool(true))
        case _                    => false
    }

  /** URIs named by `notifications/resources/updated`. */
  def resourceUpdates: ZStream[Any, Nothing, String] = ZStream.fromHub(updates)

  def subscribeResource(uri: String): IO[McpError, Unit] =
    session.request(ClientRequest.SubscribeResource(uri)).unit

  def unsubscribeResource(uri: String): IO[McpError, Unit] =
    session.request(ClientRequest.UnsubscribeResource(uri)).unit

  def readResource(uri: String): IO[McpError, Chunk[ResourceContents]] =
    session.readResource(uri)
end AppBridge

object AppBridge:
  /** What this view is and asks for at `ui/initialize`. `onTeardown` runs before the view answers the host's
    * `ui/resource-teardown`, so the view can save what it must before it goes.
    */
  final case class Settings(
      app: Implementation,
      capabilities: AppCapabilities = AppCapabilities(),
      requestTimeout: Duration = 30.seconds,
      onTeardown: UIO[Unit] = ZIO.unit,
  )

  /** Connects over `port`: the `ui/initialize` handshake, then `ui/notifications/initialized`. The bridge answers and
    * routes messages until the scope closes.
    */
  def connect[L <: Grant[?, ?, ?], N <: Tuple, V <: Tuple, In, Err, Out](
      shed: Shed[L, N, V],
      port: ViewPort,
      settings: Settings,
  )(using launch: L <:< Grant[In, Err, Out]): ZIO[Scope, McpError, AppBridge[In, Err, Out, N, V]] =
    val endpoint = launch(shed.launch).endpoint
    for
      ids      <- Ref.make(0L)
      pending  <- Ref.make(Map.empty[RequestId, Promise[McpError, Json.Obj]])
      runs     <- SubscriptionRef.make[Run[In, Err, Out]](Run.waiting)
      contexts <- SubscriptionRef.make(HostContext.empty)
      updates  <- Hub.sliding[String](16)
      session = HostSession(port, ids, pending, settings.requestTimeout)
      _ <- port.receive
        .foreach {
          case Message.Result(id, result)           => session.settle(id, Right(result))
          case Message.Error(Some(id), e)           => session.settle(id, Left(McpError.Rpc(e)))
          case Message.Error(None, _)               => ZIO.unit
          case Message.Notification(method, params) =>
            if method == Notifications.ResourceUpdated then
              params.get("uri") match
                case Some(Json.Str(uri)) => updates.publish(uri).unit
                case _                   => ZIO.unit
            else
              HostNotification.decode(method, params) match
                case Right(HostNotification.HostContextChanged(patch)) => contexts.update(_.merge(patch))
                case Right(note) => runs.get.flatMap(Run.step(endpoint)(_, note)).flatMap(runs.set)
                case Left(_)     => ZIO.unit // a notification a newer host may send
          case Message.Request(id, method, params) =>
            HostRequest.decode(method, params) match
              case Right(HostRequest.ResourceTeardown(_)) =>
                settings.onTeardown *> port.send(Message.Result(id, Json.Obj()))
              case Left(_) if method == "ping" => port.send(Message.Result(id, Json.Obj()))
              case Left(_)                     => port.send(Message.Error(Some(id), RpcError.methodNotFound(method)))
        }
        .ensuring(session.closeAll)
        .forkScoped
      init <- session.ask(ViewRequest.Initialize(settings.app, settings.capabilities))
      host <- ZIO.fromEither(init.as[InitializeResult]).mapError(McpError.Protocol(_))
      _    <- contexts.set(host.hostContext)
      _    <- session.tell(ViewNotification.Initialized)
    yield AppBridge(shed, host, session, runs, contexts, updates)
    end for
  end connect

  /** The host, seen as an MCP peer that answers `tools/call`, `resources/read`, and `ping` over the view's port. */
  private[ui] final class HostSession(
      port: ViewPort,
      ids: Ref[Long],
      pending: Ref[Map[RequestId, Promise[McpError, Json.Obj]]],
      timeout: Duration,
  ) extends McpSession:
    def era: Era                       = Era.Session
    def server: Option[Implementation] = None

    def request(req: ClientRequest): IO[McpError, Json.Obj] = send(req.method, req.params)

    def ask(req: ViewRequest): IO[McpError, Json.Obj] = send(req.method, req.params)

    def tell(note: ViewNotification): IO[McpError, Unit] = port.send(note.message)

    private def send(method: String, params: Json.Obj): IO[McpError, Json.Obj] =
      for
        id     <- ids.updateAndGet(_ + 1).map(RequestId.Num(_))
        answer <- Promise.make[McpError, Json.Obj]
        _      <- pending.update(_ + (id -> answer))
        _      <- port.send(Message.Request(id, method, params))
        result <- answer.await.timeoutFail(McpError.TimedOut(method))(timeout).ensuring(pending.update(_ - id))
      yield result

    def settle(id: RequestId, answer: Either[McpError, Json.Obj]): UIO[Unit] =
      pending.get.flatMap(open => ZIO.foreachDiscard(open.get(id))(_.complete(ZIO.fromEither(answer))))

    /** The port ended: every request still waiting fails, so none waits out its timeout. */
    val closeAll: UIO[Unit] = pending.get.flatMap(open => ZIO.foreachDiscard(open.values)(_.fail(McpError.Closed)))
  end HostSession
end AppBridge
