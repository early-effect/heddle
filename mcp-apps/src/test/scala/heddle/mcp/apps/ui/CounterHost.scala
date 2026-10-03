package heddle.mcp.apps.ui

import heddle.*
import heddle.mcp.Mcp
import heddle.mcp.apps.{Count, Grant, Shed, UiDocument, UiUri, withApp}

import heddle.mcp.client.{McpClient, McpError, McpSession}
import heddle.mcp.protocol.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** A counter shed, a real heddle MCP server for it, and a fake host between a view and that server. */
object CounterHost:
  val show  = Endpoint.get("counter").out[Count].name("show_counter").summary("Open the counter")
  val inc   = Endpoint.post("counter" / "inc").out[Count].name("inc").summary("Increment")
  val reset = Endpoint.post("counter" / "reset").out[Count].name("reset")

  val shed =
    Shed(UiUri("ui://counter/view"), "Counter", Grant.launch(show))((inc = Grant.app(inc), reset = Grant.app(reset)))

  val hostInfo = Implementation("test-host", "1")
  val start    = HostContext(theme = Some(Theme.Light), displayMode = Some(DisplayMode.Inline))

  private def json[A: JsonEncoder](a: A): Json.Obj =
    a.toJsonAST.toOption.collect { case o: Json.Obj => o }.getOrElse(Json.Obj())

  /** A real heddle MCP server for the shed, reached in memory: the upstream a host forwards `tools/call` to. */
  val upstream: ZIO[Scope, Any, McpSession] =
    for
      n <- Ref.make(0)
      api = Api("Counter", "1.0.0")
        .job(show)(_ => n.get.map(Count(_)))
        .resource(inc)(_ => n.updateAndGet(_ + 1).map(Count(_)))
        .resource(reset)(_ => n.set(0).as(Count(0)))
      mcp <- ZIO.fromEither(Mcp.from(api))
      app <- ZIO.fromEither(mcp.withApp(shed, UiDocument("Counter", "0")))
      s   <- McpClient
        .http("http://counter.test/mcp", McpClient.Settings(hostInfo))
        .provideSome[Scope](Client.inMemory(app.routes))
    yield s

  /** The host's side: answers the view's requests, and records every message the view sent. */
  final case class Host(port: ViewPort, seen: Ref[Chunk[Message]]):
    def notify(n: HostNotification): IO[McpError, Unit] = port.send(n.message)

  def host(port: ViewPort, up: McpSession): URIO[Scope, Host] =
    Ref.make(Chunk.empty[Message]).flatMap { seen =>
      def answer(id: RequestId, method: String, params: Json.Obj): IO[McpError, Unit] =
        method match
          case "ui/initialize" =>
            port.send(
              Message.Result(id, json(InitializeResult(UiProtocol.Version, hostInfo, HostCapabilities(), start)))
            )
          case "tools/call" if params.get("name").contains(Json.Str("reset")) =>
            port.send(Message.Error(Some(id), RpcError.InvalidParams("reset is not linked to this view")))
          case "tools/call" =>
            val name = params.get("name").collect { case Json.Str(s) => s }.flatMap(ToolName.from(_).toOption)
            ZIO.foreachDiscard(name)(n =>
              up.callTool(n, Json.Obj()).flatMap(r => port.send(Message.Result(id, json(r))))
            )
          case "ui/open-link"            => port.send(Message.Result(id, Outcome.wire(Outcome.Refused)))
          case "ui/request-display-mode" => port.send(Message.Result(id, Json.Obj("mode" -> Json.Str("fullscreen"))))
          case _                         => ZIO.unit // silent, so the view's timeout is what answers
      port.receive
        .foreach { m =>
          seen.update(_ :+ m) *> (m match
            case Message.Request(id, method, params) => answer(id, method, params)
            case _                                   => ZIO.unit)
        }
        .ignore
        .forkScoped
        .as(Host(port, seen))
    }

end CounterHost
