package heddle.mcp.server

import heddle.http.header.Headers
import heddle.mcp.ServedResource
import heddle.mcp.events.EventGateway
import heddle.mcp.protocol.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, UIO, ZIO}

/** What a server offers: tools, resources, and the extensions it advertises with their settings. */
final case class Offer[-R](
    tools: Chunk[ToolCall[R]],
    resources: Chunk[ServedResource[R]],
    extensions: Map[ExtensionId, Json.Obj],
    streams: Chunk[OperationStream[R]] = Chunk.empty,
    events: EventGateway[R] = EventGateway.none,
)

/** Answers MCP requests from typed messages. Transports parse bytes and pick the [[Era]]; this decides everything else,
  * and never throws or dies on what a client sends.
  */
final class Engine[-R](
    val server: Implementation,
    val offer: Offer[R],
    val instructions: Option[String],
    val listTtlMs: Long,
    hub: SessionHub,
):
  private val byName: Map[ToolName, ToolCall[R]]          = offer.tools.map(t => t.tool.name -> t).toMap
  private val byStream: Map[ToolName, OperationStream[R]] = offer.streams.map(s => s.name -> s).toMap
  private val byUri: Map[String, ServedResource[R]]       = offer.resources.map(r => r.resource.uri -> r).toMap

  /** `tools` always; `resources` when there are any; `extensions` as `server/discover` and `initialize` send them. */
  val capabilities: Json.Obj =
    val resources = Option.when(offer.resources.nonEmpty)(
      "resources" -> Json.Obj("subscribe" -> Json.Bool(true))
    )
    val extensions = Option.when(extensionSettings.nonEmpty)(
      "extensions" -> Json.Obj(Chunk.fromIterable(extensionSettings.map((id, settings) => id.value -> settings)))
    )
    val subscriptions = Option.when(offer.streams.nonEmpty)(
      "subscriptions" -> Json.Arr(offer.streams.map { stream =>
        Json.Obj(
          "name"          -> Json.Str(stream.name.value),
          "inputSchema"   -> stream.inputSchema,
          "elementSchema" -> stream.elementSchema,
        )
      })
    )
    Json.Obj(
      Chunk("tools" -> Json.Obj()) ++
        Chunk.fromIterable(resources) ++
        Chunk.fromIterable(subscriptions) ++
        Chunk.fromIterable(extensions)
    )

  /** One `subscriptions/listen`. `Left` is a JSON-RPC error and no acknowledgement. `Right` starts with the
    * acknowledgement, then one message per fact, and ends with the complete result when the pull ends.
    */
  def openListen(
      id: RequestId,
      params: Json.Obj,
      headers: Headers,
      era: Era,
  ): ZIO[R, Nothing, Either[Message, ZStream[R, Nothing, Message]]] =
    admit(params, era) match
      case Left(e)  => ZIO.left(Message.Error(Some(id), e))
      case Right(_) =>
        ClientRequest.decode(Methods.Listen, params) match
          case Left(e)                                 => ZIO.left(Message.Error(Some(id), e))
          case Right(ClientRequest.Listen(name, args)) =>
            byStream.get(name) match
              case None =>
                ZIO.left(Message.Error(Some(id), RpcError.InvalidParams(s"Unknown operation: ${name.value}")))
              case Some(stream) =>
                stream
                  .pull(args, headers)
                  .foldZIO(
                    e => ZIO.left(Message.Error(Some(id), e)),
                    facts => ZIO.right(frames(id, era, facts)),
                  )
          case Right(_) =>
            ZIO.left(Message.Error(Some(id), RpcError.InvalidRequest("subscriptions/listen is a stream")))

  /** Parses and answers one JSON value. Notifications and stray responses get no reply. */
  def handle(raw: Json, headers: Headers, era: Era, session: Option[String]): ZIO[R, Nothing, Option[Message]] =
    Message.decode(raw).fold(e => ZIO.some(e), respond(_, headers, era, session))

  def respond(msg: Message, headers: Headers, era: Era, session: Option[String]): ZIO[R, Nothing, Option[Message]] =
    msg match
      case Message.Request(id, method, params) =>
        answer(method, params, headers, era, session).map(reply(id, era, _)).asSome
      case Message.Notification(method, params) =>
        noted(method, params, session).as(None)
      case _ => ZIO.none

  /** `notifications/cancelled` interrupts that request. Any other notification, including a cancel with a bad body, is
    * ignored. A notification never becomes a response.
    */
  private def noted(method: String, params: Json.Obj, session: Option[String]): UIO[Unit] =
    method match
      case Notifications.Cancelled =>
        ZIO.foreachDiscard(Cancelled.from(params))(c => hub.interrupt(session, c.requestId))
      case _ => ZIO.unit

  private def reply(id: RequestId, era: Era, out: Either[RpcError, Json.Obj]): Message =
    out match
      case Left(e)     => Message.Error(Some(id), e)
      case Right(body) =>
        era match
          case Era.Stateless => Message.Result(id, Envelope.withServer(body, server))
          case Era.Session   => Message.Result(id, Envelope.forSession(body))

  private def answer(
      method: String,
      params: Json.Obj,
      headers: Headers,
      era: Era,
      session: Option[String],
  ): ZIO[R, Nothing, Either[RpcError, Json.Obj]] =
    admit(params, era).flatMap(_ => ClientRequest.decode(method, params)) match
      case Left(e)    => ZIO.left(e)
      case Right(req) => serve(req, headers, era, session)

  /** 2026 requests must name the one revision this server speaks. */
  private def admit(params: Json.Obj, era: Era): Either[RpcError, Unit] =
    era match
      case Era.Session   => Right(())
      case Era.Stateless =>
        RequestMeta.of(params).protocolVersion match
          case None                                    => Left(RpcError.InvalidParams("missing protocol version"))
          case Some(v) if v != ProtocolVersion.Current =>
            Left(RpcError.UnsupportedVersion(v.value, Chunk(ProtocolVersion.Current)))
          case Some(_) => Right(())

  private def serve(
      req: ClientRequest,
      headers: Headers,
      era: Era,
      session: Option[String],
  ): ZIO[R, Nothing, Either[RpcError, Json.Obj]] =
    (req, era) match
      case (ClientRequest.Discover, Era.Stateless)          => ZIO.right(discover)
      case (ClientRequest.Initialize(_, _, _), Era.Session) =>
        ZIO.right(Envelope.initialize(server, capabilities, instructions))
      case (ClientRequest.Ping, _)                 => ZIO.right(Envelope.complete())
      case (ClientRequest.ListTools(_), _)         => ZIO.right(listTools)
      case (ClientRequest.CallTool(name, args), _) =>
        byName.get(name) match
          case None       => ZIO.left(RpcError.InvalidParams(s"Unknown tool: ${name.value}"))
          case Some(tool) => tool.call(args, headers).map(r => Right(Envelope.complete(encoded(r))))
      case (ClientRequest.ListResources(_), _) =>
        ZIO.right(listed("resources", Json.Arr(offer.resources.map(r => encoded(r.resource)))))
      case (ClientRequest.ListResourceTemplates(_), _) =>
        ZIO.right(listed("resourceTemplates", Json.Arr()))
      case (ClientRequest.ReadResource(uri), _)                => read(uri)
      case (ClientRequest.SubscribeResource(uri), Era.Session) =>
        membership(session, uri, subscribe = true, "resources/subscribe")
      case (ClientRequest.UnsubscribeResource(uri), Era.Session) =>
        membership(session, uri, subscribe = false, "resources/unsubscribe")
      case (ClientRequest.SubscribeResource(_), Era.Stateless) =>
        ZIO.left(RpcError.InvalidRequest("resources/subscribe needs a session"))
      case (ClientRequest.UnsubscribeResource(_), Era.Stateless) =>
        ZIO.left(RpcError.InvalidRequest("resources/unsubscribe needs a session"))
      case (ClientRequest.Listen(_, _), _) =>
        ZIO.left(RpcError.InvalidRequest("subscriptions/listen is a stream"))
      case (ClientRequest.ListEvents(cursor), Era.Stateless)       => offer.events.list(cursor)
      case (poll: ClientRequest.PollEvents, Era.Stateless)         => offer.events.poll(headers, poll)
      case (sub: ClientRequest.SubscribeEvents, Era.Stateless)     => offer.events.subscribe(headers, sub)
      case (unsub: ClientRequest.UnsubscribeEvents, Era.Stateless) => offer.events.unsubscribe(headers, unsub)
      case (ClientRequest.StreamEvents(_, _, _, _, _), Era.Stateless) if offer.events.enabled =>
        ZIO.left(
          RpcError.Unsupported(Json.Obj("feature" -> Json.Str("deliveryMode"), "value" -> Json.Str("push")))
        )
      case (other, _) => ZIO.left(RpcError.methodNotFound(other.method))

  private def read(uri: String): ZIO[R, Nothing, Either[RpcError, Json.Obj]] =
    byUri.get(uri) match
      case None         => ZIO.left(RpcError.ResourceNotFound(s"Resource not found: $uri"))
      case Some(served) =>
        served.read.fold(
          e => Left(RpcError.Internal(s"Resource $uri is unavailable: ${e.reason}")),
          contents => Right(Envelope.complete(encoded(ReadResourceResult(contents)))),
        )

  /** Subscribe requires the resource. Unsubscribe of a missing URI is still success: the client is already done. */
  private def membership(
      session: Option[String],
      uri: String,
      subscribe: Boolean,
      method: String,
  ): ZIO[R, Nothing, Either[RpcError, Json.Obj]] =
    session match
      case None     => ZIO.left(RpcError.InvalidRequest(s"$method needs a session"))
      case Some(id) =>
        if subscribe && !byUri.contains(uri) then ZIO.left(RpcError.ResourceNotFound(s"Resource not found: $uri"))
        else
          val changed = if subscribe then hub.subscribe(id, uri) else hub.unsubscribe(id, uri)
          changed.map {
            case false => Left(RpcError.InvalidRequest("unknown session"))
            case true  => Right(Envelope.complete())
          }

  private def frames(id: RequestId, era: Era, facts: ZStream[R, Nothing, StreamFact]): ZStream[R, Nothing, Message] =
    ZStream.succeed(acknowledged(id)) ++
      facts.map {
        case StreamFact.Event(appId, data) => operationEvent(id, appId, data)
        case StreamFact.Failed(data)       => operationError(id, data)
      } ++
      ZStream.succeed(listenResult(id, era))

  private def acknowledged(id: RequestId): Message =
    Message.Notification(Notifications.SubscriptionsAcknowledged, Json.Obj("_meta" -> subscriptionMeta(id)))

  private def operationEvent(id: RequestId, appId: String, data: Json): Message =
    Message.Notification(
      Notifications.OperationEvent,
      Json.Obj("_meta" -> subscriptionMeta(id), "id" -> Json.Str(appId), "data" -> data),
    )

  private def operationError(id: RequestId, data: Option[Json]): Message =
    Message.Notification(
      Notifications.OperationError,
      Json.Obj(Chunk("_meta" -> subscriptionMeta(id)) ++ Chunk.fromIterable(data.map("data" -> _))),
    )

  /** Keeps `subscriptionId` on the result for both eras. A session result otherwise strips `_meta`. */
  private def listenResult(id: RequestId, era: Era): Message =
    val info = era match
      case Era.Stateless =>
        Chunk(RequestMeta.ServerInfoKey -> server.toJsonAST.getOrElse(Json.Obj()))
      case Era.Session => Chunk.empty
    val meta = Json.Obj(Chunk(RequestMeta.SubscriptionIdKey -> id.json) ++ info)
    Message.Result(id, Json.Obj("resultType" -> Json.Str("complete"), "_meta" -> meta))

  private def subscriptionMeta(id: RequestId): Json.Obj =
    Json.Obj(RequestMeta.SubscriptionIdKey -> id.json)

  /** A server with events always advertises the empty settings object, so a later `withExtension` cannot add
    * `listChanged`.
    */
  private def extensionSettings: Map[ExtensionId, Json.Obj] =
    if offer.events.enabled then offer.extensions.updated(ExtensionId.Events, Json.Obj())
    else offer.extensions

  private def discover: Json.Obj =
    Envelope.complete(
      Chunk(
        "supportedVersions" -> Json.Arr(Json.Str(ProtocolVersion.Current.value)),
        "capabilities"      -> capabilities,
        "ttlMs"             -> Json.Num(listTtlMs),
        "cacheScope"        -> Json.Str("public"),
      ) ++ Chunk.fromIterable(instructions.map("instructions" -> Json.Str(_)))*
    )

  private def listTools: Json.Obj =
    listed("tools", Json.Arr(offer.tools.map(t => encoded(t.tool))))

  private def listed(key: String, items: Json.Arr): Json.Obj =
    Envelope.complete(key -> items, "ttlMs" -> Json.Num(listTtlMs), "cacheScope" -> Json.Str("public"))

  private def encoded[A: JsonEncoder](a: A): Json.Obj =
    a.toJsonAST match
      case Right(o: Json.Obj) => o
      case _                  => Json.Obj()
end Engine
