package heddle.mcp.apps.host

import heddle.*
import heddle.http.Url
import heddle.mcp.{Mcp, McpBuildError}
import heddle.mcp.apps.{AppBuildError, Count, HostPolicy, UiDocument, UiPolicy, withApp}
import heddle.mcp.apps.ui.{AppCapabilities, CounterHost, ViewPort, ViewRequest}
import heddle.mcp.client.{McpClient, McpError}
import heddle.mcp.protocol.*
import zio.*
import zio.json.ast.Json

/** A real heddle MCP server for the counter shed, plus a `bash` tool the model may call and no view may, reached in
  * memory; and the recording doubles a host is tested with.
  */
object HostFixtures:
  import CounterHost.{inc, reset, show, shed}

  /** A model tool with no view: a job, so it is listed for the model, and linked to no `ui://`. */
  val bash = Endpoint.post("bash").out[String].name("bash")

  val counter: ServerName = ServerName("counter")

  type BuildError = NonEmptyChunk[McpBuildError] | NonEmptyChunk[AppBuildError] | McpError

  /** The server's count, and a session to it that serves `document` for the view. */
  def server(document: UiDocument, policy: UiPolicy = UiPolicy.closed): ZIO[Scope, BuildError, (AppServer, Ref[Int])] =
    for
      n <- Ref.make(0)
      api = Api("Counter", "1.0.0")
        .job(show)(_ => n.get.map(Count(_)))
        .resource(inc)(_ => n.updateAndGet(_ + 1).map(Count(_)))
        .resource(reset)(_ => n.set(0).as(Count(0)))
        .job(bash)(_ => ZIO.succeed("ran"))
      mcp <- ZIO.fromEither(Mcp.from(api))
      app <- ZIO.fromEither(mcp.withApp(shed.withPolicy(policy), document))
      s   <- McpClient
        .http("http://counter.test/mcp", McpClient.Settings(CounterHost.hostInfo))
        .provideSome[Scope](Client.inMemory(app.routes))
    yield (AppServer(counter, s), n)

  val view: UiDocument = UiDocument("Counter", "window.counter = 1;")

  /** The model's call of the launch tool, as the host saw it. */
  def launched(s: AppServer): IO[McpError, Launched] =
    s.session.callTool(ToolName("show_counter"), Json.Obj()).map(Launched(ToolName("show_counter"), Json.Obj(), _))

  val settings: HostSettings = HostSettings(Implementation("test-host", "1"), HostPolicy.open)

  /** A consent gate that answers `outcome` and remembers what it was asked. */
  def answering(outcome: ConsentOutcome): UIO[(ConsentGate, Ref[Chunk[ConsentRequest]])] =
    Ref.make(Chunk.empty[ConsentRequest]).map { asked =>
      val gate = new ConsentGate:
        def decide(request: ConsentRequest): UIO[ConsentOutcome] = asked.update(_ :+ request).as(outcome)
      (gate, asked)
    }

  /** A host page that remembers how it was asked to size the frame and which links it opened. */
  final case class Page(heights: Ref[Chunk[Double]], links: Ref[Chunk[Url]]) extends ViewFrame:
    def resize(height: Double): UIO[Unit] = heights.update(_ :+ height)
    def openLink(url: Url): UIO[Unit]     = links.update(_ :+ url)

  val page: UIO[Page] = (Ref.make(Chunk.empty[Double]) <*> Ref.make(Chunk.empty[Url])).map(Page(_, _))

  /** What a view that speaks JSON-RPC itself says first. */
  val initialize: Json.Obj = ViewRequest.Initialize(Implementation("raw-view", "1"), AppCapabilities()).params

  /** The view's end of a raw port, for a view that speaks JSON-RPC itself. `heard` is every message the host sent it;
    * `requests` are the host's requests, in order.
    */
  final case class RawView(
      port: ViewPort,
      heard: Ref[Chunk[Message]],
      requests: Queue[Message.Request],
      answers: Ref[Map[RequestId, Promise[Nothing, Message]]],
  ):
    /** Sends a request and waits for the host's result or error. */
    def ask(id: Long, method: String, params: Json.Obj = Json.Obj()): IO[McpError, Message] =
      for
        answer <- Promise.make[Nothing, Message]
        _      <- answers.update(_.updated(RequestId.Num(id), answer))
        _      <- port.send(Message.Request(RequestId.Num(id), method, params))
        reply  <- answer.await
      yield reply

    def notify(method: String, params: Json.Obj = Json.Obj()): IO[McpError, Unit] =
      port.send(Message.Notification(method, params))

    /** Answers a request the host sent. */
    def reply(id: RequestId): IO[McpError, Unit] = port.send(Message.Result(id, Json.Obj()))

    /** The notifications the host has sent, by method. */
    def notified: UIO[Chunk[String]] = heard.get.map(_.collect { case Message.Notification(m, _) => m })
  end RawView

  /** Two ends: the raw view's, and the host's to serve. */
  val rawPair: URIO[Scope, (RawView, ViewPort)] =
    for
      (viewEnd, hostEnd) <- ViewPort.pair
      heard              <- Ref.make(Chunk.empty[Message])
      requests           <- Queue.unbounded[Message.Request]
      answers            <- Ref.make(Map.empty[RequestId, Promise[Nothing, Message]])
      settle = (id: RequestId, m: Message) => answers.get.flatMap(a => ZIO.foreachDiscard(a.get(id))(_.succeed(m)))
      _ <- viewEnd.receive
        .foreach { m =>
          heard.update(_ :+ m) *> (m match
            case r: Message.Request         => requests.offer(r).unit
            case Message.Result(id, _)      => settle(id, m)
            case Message.Error(Some(id), _) => settle(id, m)
            case _                          => ZIO.unit)
        }
        .ignore
        .forkScoped
    yield (RawView(viewEnd, heard, requests, answers), hostEnd)
end HostFixtures
