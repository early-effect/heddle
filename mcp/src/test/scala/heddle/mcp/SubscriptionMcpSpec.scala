package heddle.mcp

import heddle.*
import heddle.mcp.protocol.*
import heddle.mcp.transport.Http
import heddle.sse.SseCodec
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*

/** `subscriptions/listen` is one pull of a stream operation. It is not a tool. */
object SubscriptionMcpSpec extends ZIOSpecDefault:
  final case class Tick(n: Int) derives Schema, JsonCodec

  enum Miss derives Schema, JsonCodec:
    case Gone

  private val ticks =
    Endpoint.get("ticks").outError[Miss](Status.NotFound).stream[Tick]

  private def mcp(open: Unit => ZIO[Any, Miss, ZStream[Any, Miss, Observed[Tick]]]) =
    ZIO.fromEither(Mcp.from(Api("ticks", "1").stream(ticks)(open)))

  private def stateless(id: Long, req: ClientRequest): Message.Request =
    Message.stateless(RequestId.Num(id), req)

  private def listen(id: Long, operation: ToolName, args: Json.Obj = Json.Obj(), accept: Boolean = true): Request =
    val msg  = stateless(id, ClientRequest.Listen(operation, args))
    val base = Request
      .post("/mcp", Body.json(msg.json.toJson))
      .withHeader(Http.ProtocolHeader, ProtocolVersion.Current.value)
      .withHeader(Http.MethodHeader, Methods.Listen)
    if accept then base.withHeader("Accept", "text/event-stream") else base

  private def raw(id: Long, method: String, params: Json.Obj): Request =
    val meta = Json.Obj(
      RequestMeta.VersionKey    -> Json.Str(ProtocolVersion.Current.value),
      RequestMeta.ClientCapsKey -> Json.Obj(),
    )
    val msg = Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> Json.Num(id),
      "method"  -> Json.Str(method),
      "params"  -> Json.Obj(params.fields :+ ("_meta" -> meta)),
    )
    Request
      .post("/mcp", Body.json(msg.toJson))
      .withHeader(Http.ProtocolHeader, ProtocolVersion.Current.value)
      .withHeader(Http.MethodHeader, method)
      .withHeader("Accept", "text/event-stream")

  private def decoded(data: String): Option[Message] =
    data.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption)

  private def messages(res: Response): ZIO[Any, Throwable, Chunk[Message]] =
    SseCodec.stream(res.body.toStream).runCollect.map { events =>
      events.flatMap(event => decoded(event.data).fold(Chunk.empty[Message])(Chunk(_)))
    }

  private def metaId(obj: Json.Obj): Option[Json] =
    obj.get("_meta").collect { case meta: Json.Obj => meta }.flatMap(_.get(RequestMeta.SubscriptionIdKey))

  private def acknowledged(msg: Message, id: Long): Boolean =
    msg match
      case Message.Notification(Notifications.SubscriptionsAcknowledged, params) =>
        metaId(params).contains(Json.Num(id))
      case _ => false

  private def event(msg: Message, sub: Long, app: String, n: Int): Boolean =
    msg match
      case Message.Notification(Notifications.OperationEvent, params) =>
        metaId(params).contains(Json.Num(sub)) &&
        params.get("id").contains(Json.Str(app)) &&
        params.get("data").contains(Json.Obj("n" -> Json.Num(n)))
      case _ => false

  private def failed(msg: Message, id: Long): Boolean =
    msg match
      case Message.Notification(Notifications.OperationError, params) =>
        metaId(params).contains(Json.Num(id)) && params.get("data").exists(_.toJson.contains("Gone"))
      case _ => false

  private def completed(msg: Message, id: Long): Boolean =
    msg match
      case Message.Result(RequestId.Num(`id`), result) =>
        result.get("resultType").contains(Json.Str("complete")) && metaId(result).contains(Json.Num(id))
      case _ => false

  private def jsonOf(msg: Option[Json]): String =
    msg.fold("")(_.toJson)

  private def isNotPromotable(
      built: Either[NonEmptyChunk[McpBuildError], Mcp[?]],
      name: String,
      reason: OpArgsError,
  ): Boolean =
    built match
      case Left(errors) =>
        errors.exists {
          case McpBuildError.NotPromotable(`name`, `reason`) => true
          case _                                             => false
        }
      case Right(_) => false

  def spec = suite("subscriptions/listen")(
    test("a stream operation is absent from tools and present in discover"):
      for
        server  <- mcp(_ => ZIO.succeed(ZStream.empty))
        catalog <- ZIO.fromEither(server.withCatalog)
        listed  <- catalog.handle(stateless(1, ClientRequest.ListTools(None)).json)
        called  <- catalog.handle(stateless(2, ClientRequest.CallTool(ToolName("get_ticks"), Json.Obj())).json)
        found   <- catalog.handle(
          stateless(
            3,
            ClientRequest.CallTool(ToolName("search_operations"), Json.Obj("query" -> Json.Str("ticks"))),
          ).json
        )
        disc <- server.handle(stateless(4, ClientRequest.Discover).json)
      yield
        val tools    = jsonOf(listed)
        val call     = jsonOf(called)
        val search   = jsonOf(found)
        val discover = jsonOf(disc)
        assertTrue(
          !tools.contains("get_ticks"),
          call.contains("Unknown tool"),
          !search.contains("get_ticks"),
          discover.contains("get_ticks"),
          discover.contains("\"subscriptions\""),
          discover.contains("inputSchema"),
          discover.contains("elementSchema"),
        )
    ,
    test("a promoted stream fails to build, and so does a shared name or a non-JSON input"):
      val promoted = Endpoint.get("ticks").mcp.stream[Tick]
      val tool     = Endpoint.get("ticks").out[Tick].mcp
      val upload   = Endpoint.post("up").in[Octet].stream[Tick]
      val shared   = Mcp.from(
        Api("ticks", "1").bind(tool)(_ => ZIO.succeed(Tick(1))),
        Api("ticks", "1").stream(ticks)(_ => ZIO.succeed(ZStream.empty)),
      )
      val marked = Mcp.from(Api("ticks", "1").stream(promoted)(_ => ZIO.succeed(ZStream.empty)))
      val binary = Mcp.from(Api("ticks", "1").stream(upload)(_ => ZIO.succeed(ZStream.empty)))
      assertTrue(
        isNotPromotable(marked, "get_ticks", OpArgsError.Subscription),
        shared match
          case Left(errors) =>
            errors.exists {
              case McpBuildError.DuplicateTool(name) => name.value == "get_ticks"
              case _                                 => false
            }
          case Right(_) => false
        ,
        isNotPromotable(binary, "post_up", OpArgsError.NonJsonBody),
      )
    ,
    test("listen needs Accept: text/event-stream"):
      for
        server <- mcp(_ => ZIO.succeed(ZStream.empty))
        bare   <- server.routes(listen(1, ToolName("get_ticks"), accept = false))
        text   <- bare.body.utf8
      yield assertTrue(
        bare.status == Status.BadRequest,
        !bare.headers.contentType.exists(_.isEventStream),
        text.contains("text/event-stream"),
      )
    ,
    test("a missing or unknown operation is invalid params and does not acknowledge"):
      for
        server  <- mcp(_ => ZIO.succeed(ZStream.succeed(Observed(SseField.of(1), Tick(1)))))
        missing <- server.routes(raw(2, Methods.Listen, Json.Obj()))
        unknown <- server.routes(listen(3, ToolName("nope")))
        missed  <- messages(missing)
        missed0 = missed.lift(0)
        named <- messages(unknown)
        named0 = named.lift(0)
      yield assertTrue(
        missed.length == 1,
        missed0.exists {
          case Message.Error(Some(RequestId.Num(2)), _: RpcError.InvalidParams) => true; case _ => false
        },
        !missed.exists(acknowledged(_, 2)),
        named.length == 1,
        named0.exists {
          case Message.Error(Some(RequestId.Num(3)), _: RpcError.InvalidParams) => true; case _ => false
        },
        !named.exists(acknowledged(_, 3)),
      )
    ,
    test("a gate failure is a JSON-RPC error and does not acknowledge"):
      for
        server <- mcp(_ => ZIO.fail(Miss.Gone))
        res    <- server.routes(listen(4, ToolName("get_ticks")))
        got    <- messages(res)
        first = got.lift(0)
      yield assertTrue(
        got.length == 1,
        first.exists {
          case Message.Error(Some(RequestId.Num(4)), RpcError.OperationFailed(data)) =>
            data.exists(_.toJson.contains("Gone"))
          case _ => false
        },
        !got.exists(acknowledged(_, 4)),
      )
    ,
    test("listen acknowledges, then the event carries the request id and the application id"):
      val followed = Endpoint.get("jobs" / string("session")).query[Long]("after").stream[Tick]
      for
        seen   <- Ref.make("")
        server <- ZIO.fromEither(
          Mcp.from(
            Api("ticks", "1").stream(followed) { (session, after) =>
              seen.set(s"$session:$after") *>
                ZIO.succeed(ZStream.succeed(Observed(SseField.of(after), Tick(after.toInt))))
            }
          )
        )
        res <- server.routes(
          listen(4, ToolName("get_jobs_session"), Json.Obj("session" -> Json.Str("abc"), "after" -> Json.Num(12)))
        )
        got  <- messages(res)
        what <- seen.get
      yield assertTrue(
        what == "abc:12",
        got.length == 3,
        got.lift(0).exists(acknowledged(_, 4)),
        got.lift(1).exists(event(_, 4, "12", 12)),
        got.lift(2).exists(completed(_, 4)),
      )
    ,
    test("a later failure is one error notification, then the complete result"):
      for
        server <- mcp(_ => ZIO.succeed(ZStream.succeed(Observed(SseField.of(4), Tick(4))) ++ ZStream.fail(Miss.Gone)))
        res    <- server.routes(listen(5, ToolName("get_ticks")))
        got    <- messages(res)
      yield assertTrue(
        got.length == 4,
        got.lift(0).exists(acknowledged(_, 5)),
        got.lift(1).exists(event(_, 5, "4", 4)),
        got.lift(2).exists(failed(_, 5)),
        got.lift(3).exists(completed(_, 5)),
      )
    ,
    test("cancel stops that pull only"):
      for
        hub    <- Hub.unbounded[Observed[Tick]]
        ready  <- Queue.unbounded[Unit]
        closed <- Ref.make(0)
        server <- mcp { _ =>
          ZIO.succeed(
            ZStream
              .scoped(hub.subscribe.tap(_ => ready.offer(())))
              .flatMap(queue => ZStream.fromQueue(queue).ensuring(closed.update(_ + 1)))
          )
        }
        first  <- server.routes(listen(1, ToolName("get_ticks")))
        second <- server.routes(listen(2, ToolName("get_ticks")))
        left   <- Queue.unbounded[Message]
        right  <- Queue.unbounded[Message]
        _      <- messagesInto(first, left).fork
        _      <- messagesInto(second, right).fork
        _      <- ready.take *> ready.take
        ackL   <- left.take
        ackR   <- right.take
        _      <- hub.publish(Observed(SseField.of(1), Tick(1)))
        oneL   <- left.take
        oneR   <- right.take
        _      <- server.routes(cancel(RequestId.Num(1)))
        gone   <- closed.get
        _      <- hub.publish(Observed(SseField.of(2), Tick(2)))
        twoR   <- right.take
        extra  <- left.poll
      yield assertTrue(
        acknowledged(ackL, 1),
        acknowledged(ackR, 2),
        event(oneL, 1, "1", 1),
        event(oneR, 2, "1", 1),
        gone == 1,
        event(twoR, 2, "2", 2),
        extra.isEmpty,
      )
    ,
    test("stdio emits the acknowledgement, the event, and the complete result"):
      val line =
        stateless(4, ClientRequest.Listen(ToolName("get_ticks"), Json.Obj())).json.toJson + "\n"
      val in  = java.io.ByteArrayInputStream(line.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      val out = java.io.ByteArrayOutputStream()
      for
        server <- mcp(_ => ZIO.succeed(ZStream.succeed(Observed(SseField.of(7), Tick(7)))))
        _      <- server.stdio(in, out)
      yield
        val text = String(out.toByteArray, java.nio.charset.StandardCharsets.UTF_8)
        assertTrue(
          text.contains(Notifications.SubscriptionsAcknowledged),
          text.contains(Notifications.OperationEvent),
          text.contains(RequestMeta.SubscriptionIdKey),
          text.contains(""""id":"7""""),
          text.contains(""""n":7"""),
          text.contains("\"resultType\":\"complete\""),
          !text.contains("tools/list"),
        )
    ,
    test("a session listen keeps the subscription id on the result"):
      for
        server <- mcp(_ => ZIO.succeed(ZStream.succeed(Observed(SseField.of(1), Tick(1)))))
        init   <- server.routes(initialize)
        sid    <- ZIO.fromOption(init.header(Http.SessionHeader)).orElseFail(())
        res    <- server.routes(sessionListen(sid, 8))
        got    <- messages(res)
      yield assertTrue(
        init.status == Status.Ok,
        got.lift(0).exists(acknowledged(_, 8)),
        got.lift(1).exists(event(_, 8, "1", 1)),
        got.lift(2).exists(completed(_, 8)),
      ),
  )

  private def messagesInto(res: Response, queue: Queue[Message]): ZIO[Any, Throwable, Unit] =
    SseCodec.stream(res.body.toStream).foreach { event =>
      decoded(event.data) match
        case Some(msg) => queue.offer(msg).unit
        case None      => ZIO.unit
    }

  private def cancel(id: RequestId): Request =
    Request
      .post("/mcp", Body.json(Cancelled(id, Some("stop")).message.json.toJson))
      .withHeader(Http.ProtocolHeader, ProtocolVersion.Current.value)
      .withHeader("Accept", "text/event-stream")

  private def initialize: Request =
    val msg = Message.request(
      RequestId.Num(1),
      ClientRequest.Initialize(ProtocolVersion.Legacy, Json.Obj(), Some(Implementation("spec", "0"))),
    )
    Request.post("/mcp", Body.json(msg.json.toJson))

  private def sessionListen(sid: String, id: Long): Request =
    val msg = Message.request(RequestId.Num(id), ClientRequest.Listen(ToolName("get_ticks"), Json.Obj()))
    Request
      .post("/mcp", Body.json(msg.json.toJson))
      .withHeader(Http.SessionHeader, sid)
      .withHeader("Accept", "text/event-stream")
end SubscriptionMcpSpec
