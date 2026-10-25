package heddle.mcp

import heddle.*
import heddle.mcp.protocol.*
import heddle.mcp.transport.Http
import heddle.sse.SseCodec
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** HTTP tool calls: notifications ride the POST that invoked the tool, and that POST is what a cancel interrupts. */
object HttpSessionSpec extends ZIOSpecDefault:
  final case class Arg(n: Int) derives Schema, JsonCodec

  private val note: Message.Notification =
    Message.Notification("notifications/progress", Json.Obj("progress" -> Json.Num(1)))

  private val args: Json.Obj = Json.Obj("n" -> Json.Num(1))

  private def mcp(run: Arg => UIO[String]): ZIO[Any, NonEmptyChunk[McpBuildError], Mcp[Any]] =
    ZIO.fromEither(Mcp.from(Api("Shop", "1")).flatMap(_.tool[Arg]("run")(run)))

  private def pair(
      hold: Arg => UIO[String],
      pass: Arg => UIO[String],
  ): ZIO[Any, NonEmptyChunk[McpBuildError], Mcp[Any]] =
    ZIO.fromEither(
      Mcp.from(Api("Shop", "1")).flatMap(_.tool[Arg]("hold")(hold)).flatMap(_.tool[Arg]("pass")(pass))
    )

  private def call(name: ToolName, id: Long, session: Option[String], sse: Boolean): Request =
    val msg =
      session match
        case None =>
          Message.stateless(RequestId.Num(id), ClientRequest.CallTool(name, args))
        case Some(_) =>
          Message.request(RequestId.Num(id), ClientRequest.CallTool(name, args))
    val version = session.fold(ProtocolVersion.Current.value)(_ => ProtocolVersion.Legacy.value)
    val base    = Request
      .post("/mcp", Body.json(msg.json.toJson))
      .withHeader(Http.ProtocolHeader, version)
    val routed =
      session match
        case None =>
          base.withHeader(Http.MethodHeader, Methods.CallTool).withHeader(Http.NameHeader, name.value)
        case Some(sid) => base.withHeader(Http.SessionHeader, sid)
    if sse then routed.withHeader("Accept", "application/json, text/event-stream") else routed
  end call

  private def initialize: Request =
    val msg = Message.request(
      RequestId.Num(1),
      ClientRequest.Initialize(ProtocolVersion.Legacy, Json.Obj(), Some(Implementation("spec", "0"))),
    )
    Request
      .post("/mcp", Body.json(msg.json.toJson))
      .withHeader("Accept", "text/event-stream")

  private def cancel(id: RequestId, session: Option[String]): Request =
    val version = session.fold(ProtocolVersion.Current.value)(_ => ProtocolVersion.Legacy.value)
    val base    = Request
      .post("/mcp", Body.json(Cancelled(id, Some("stop")).message.json.toJson))
      .withHeader(Http.ProtocolHeader, version)
      .withHeader("Accept", "text/event-stream")
    session.fold(base)(sid => base.withHeader(Http.SessionHeader, sid))

  private def decoded(data: String): Option[Message] =
    data.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption)

  private def resultIs(message: Option[Message], id: Long, marker: String): Boolean =
    message.exists {
      case Message.Result(RequestId.Num(`id`), result) => result.toJson.contains(marker)
      case _                                           => false
    }

  def spec = suite("HTTP session posts")(
    test("an event-stream POST yields the notification, then the result"):
      for
        server <- mcp(_ => Session.publish(note) *> ZIO.succeed("ok"))
        res    <- server.routes(call(ToolName("run"), 7, None, sse = true))
        events <- SseCodec.stream(res.body.toStream).runCollect
        first  = events.headOption
        second = events.drop(1).headOption
      yield assertTrue(
        res.status == Status.Ok,
        res.headers.contentType.exists(_.isEventStream),
        events.size == 2,
        first.map(_.data).contains(note.json.toJson),
        resultIs(second.flatMap(event => decoded(event.data)), 7, "ok"),
      )
    ,
    test("the same call without event-stream is one JSON result, and publish adds nothing"):
      for
        server <- mcp(_ => Session.publish(note) *> ZIO.succeed("ok"))
        res    <- server.routes(call(ToolName("run"), 7, None, sse = false).withHeader("Accept", "application/json"))
        text   <- res.body.utf8
      yield assertTrue(
        res.status == Status.Ok,
        res.headers.contentType.exists(_.isJson),
        !text.contains("notifications/progress"),
        !text.contains("data:"),
        resultIs(decoded(text), 7, "ok"),
      )
    ,
    test("a second tools/call finishes while the first is still waiting"):
      for
        gate    <- Promise.make[Nothing, Unit]
        started <- Promise.make[Nothing, Unit]
        server  <- pair(
          _ => started.succeed(()) *> gate.await *> ZIO.succeed("from-hold"),
          _ => ZIO.succeed("from-pass"),
        )
        held     <- server.routes(call(ToolName("hold"), 1, None, sse = false)).fork
        _        <- started.await
        passed   <- server.routes(call(ToolName("pass"), 2, None, sse = false))
        passText <- passed.body.utf8
        _        <- gate.succeed(())
        heldRes  <- held.join
        heldText <- heldRes.body.utf8
      yield assertTrue(
        passed.status == Status.Ok,
        resultIs(decoded(passText), 2, "from-pass"),
        heldRes.status == Status.Ok,
        resultIs(decoded(heldText), 1, "from-hold"),
      )
    ,
    test("notifications/cancelled interrupts the waiting call and leaves the other one"):
      for
        holdGate    <- Promise.make[Nothing, Unit]
        holdStarted <- Promise.make[Nothing, Unit]
        holdStopped <- Promise.make[Nothing, Unit]
        passGate    <- Promise.make[Nothing, Unit]
        passStarted <- Promise.make[Nothing, Unit]
        passStopped <- Promise.make[Nothing, Unit]
        server      <- pair(
          _ => holdStarted.succeed(()) *> holdGate.await.onInterrupt(holdStopped.succeed(())).as("from-hold"),
          _ => passStarted.succeed(()) *> passGate.await.onInterrupt(passStopped.succeed(())).as("from-pass"),
        )
        streaming  <- server.routes(call(ToolName("hold"), 4, None, sse = true))
        events     <- SseCodec.stream(streaming.body.toStream).runCollect.fork
        _          <- holdStarted.await
        other      <- server.routes(call(ToolName("pass"), 5, None, sse = false)).fork
        _          <- passStarted.await
        cancelled  <- server.routes(cancel(RequestId.Num(4), None))
        _          <- holdStopped.await
        otherStill <- passStopped.poll
        _          <- passGate.succeed(())
        otherRes   <- other.join
        otherText  <- otherRes.body.utf8
        got        <- events.join
      yield assertTrue(
        cancelled.status == Status.Accepted,
        cancelled.headers.contentType.isEmpty,
        cancelled.body.text.exists(_.isEmpty),
        otherStill.isEmpty,
        otherRes.status == Status.Ok,
        resultIs(decoded(otherText), 5, "from-pass"),
        !got.exists(event =>
          decoded(event.data).exists {
            case Message.Result(RequestId.Num(4), _) => true
            case _                                   => false
          }
        ),
      )
    ,
    test("closing the POST stream interrupts the tool"):
      for
        started <- Promise.make[Nothing, Unit]
        stopped <- Promise.make[Nothing, Unit]
        gate    <- Promise.make[Nothing, Unit]
        server  <- mcp { _ =>
          started.succeed(()) *> gate.await.onInterrupt(stopped.succeed(())).as("from-hold")
        }
        _ <- ZIO.scoped {
          for
            res <- server.routes(call(ToolName("run"), 3, None, sse = true))
            _   <- SseCodec.stream(res.body.toStream).runDrain.forkScoped
            _   <- started.await
          yield ()
        }
        _ <- stopped.await
      yield assertCompletes
    ,
    test("notifications/cancelled interrupts a JSON call, which answers 202, and the next call still runs"):
      for
        holdGate    <- Promise.make[Nothing, Unit]
        holdStarted <- Promise.make[Nothing, Unit]
        holdStopped <- Promise.make[Nothing, Unit]
        server      <- pair(
          _ => (holdStarted.succeed(()) *> holdGate.await).onInterrupt(holdStopped.succeed(())).as("from-hold"),
          _ => ZIO.succeed("from-pass"),
        )
        held      <- server.routes(call(ToolName("hold"), 11, None, sse = false)).fork
        _         <- holdStarted.await
        cancelled <- server.routes(cancel(RequestId.Num(11), None))
        _         <- holdStopped.await
        passed    <- server.routes(call(ToolName("pass"), 12, None, sse = false))
        passText  <- passed.body.utf8
        heldRes   <- held.join
        heldText  <- heldRes.body.utf8
      yield assertTrue(
        cancelled.status == Status.Accepted,
        cancelled.body.text.exists(_.isEmpty),
        heldRes.status == Status.Accepted,
        heldText.isEmpty,
        passed.status == Status.Ok,
        resultIs(decoded(passText), 12, "from-pass"),
      )
    ,
    test("DELETE interrupts the session tool and not a stateless one"):
      for
        sessionStarted <- Promise.make[Nothing, Unit]
        sessionStopped <- Promise.make[Nothing, Unit]
        sessionGate    <- Promise.make[Nothing, Unit]
        looseStarted   <- Promise.make[Nothing, Unit]
        looseStopped   <- Promise.make[Nothing, Unit]
        looseGate      <- Promise.make[Nothing, Unit]
        server         <- pair(
          _ => sessionStarted.succeed(()) *> sessionGate.await.onInterrupt(sessionStopped.succeed(())).as("session"),
          _ => looseStarted.succeed(()) *> looseGate.await.onInterrupt(looseStopped.succeed(())).as("loose"),
        )
        init <- server.routes(initialize)
        out  <- init.header(Http.SessionHeader) match
          case None =>
            ZIO.succeed(assertTrue(false))
          case Some(sid) =>
            for
              opened     <- server.routes(call(ToolName("hold"), 8, Some(sid), sse = true))
              pull       <- SseCodec.stream(opened.body.toStream).runDrain.fork
              _          <- sessionStarted.await
              loose      <- server.routes(call(ToolName("pass"), 9, None, sse = false)).fork
              _          <- looseStarted.await
              gone       <- server.routes(Request(Method.DELETE, Url.parse("/mcp")).withHeader(Http.SessionHeader, sid))
              _          <- sessionStopped.await
              looseStill <- looseStopped.poll
              _          <- looseGate.succeed(())
              looseRes   <- loose.join
              looseText  <- looseRes.body.utf8
              _          <- pull.join
              initText   <- init.body.utf8
            yield assertTrue(
              init.status == Status.Ok,
              init.headers.contentType.exists(_.isJson),
              initText.contains(ProtocolVersion.Legacy.value),
              opened.status == Status.Ok,
              opened.headers.contentType.exists(_.isEventStream),
              opened.header(Http.SessionHeader).contains(sid),
              gone.status == Status.Ok,
              looseStill.isEmpty,
              looseRes.status == Status.Ok,
              resultIs(decoded(looseText), 9, "loose"),
            )
      yield out,
  ) @@ TestAspect.timeout(10.seconds)
end HttpSessionSpec
