package heddle.mcp.transport

import heddle.http.{MediaType, Method, Request, Response, Status}
import heddle.http.header.Headers
import heddle.mcp.Session
import heddle.mcp.protocol.{Era, Message, Methods, ProtocolVersion, RequestId, RequestMeta, RpcError}
import heddle.mcp.server.{Engine, Envelope, SessionHub}
import heddle.route.{PathCodec, Route, Routes}
import heddle.sse.{ServerSentEvent, Sse}
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Exit, Fiber, Promise, Queue, Ref, Scope, ZIO, ZNothing}

/** Streamable HTTP. 2026-07-28 requests carry their method, tool name, and version in headers too, and the two must
  * agree. A request that says `initialize`, or names 2025-11-25, is a session request.
  */
object Http:
  val ProtocolHeader = "MCP-Protocol-Version"
  val MethodHeader   = "Mcp-Method"
  val NameHeader     = "Mcp-Name"
  val SessionHeader  = Envelope.SessionHeader

  def routes[R](engine: Engine[R], hub: SessionHub, path: String = "mcp"): Routes[R, ZNothing] =
    val at = literalPath(path.split('/').filter(_.nonEmpty).toList)
    // Order is the Allow list. A different path is a miss. A different method is 405 from the router.
    Routes(
      Route.from(Method.GET, at, (_, req) => get(hub, req)),
      Route.from(Method.POST, at, (_, req) => post(engine, hub, req)),
      Route.from(Method.DELETE, at, (_, req) => delete(hub, req)),
    )

  private def literalPath(segments: List[String]): PathCodec[Unit] =
    segments.foldLeft[PathCodec[Unit] { type Names = EmptyTuple }](PathCodec.empty) { (path, segment) =>
      path.appendLit[EmptyTuple](segment)
    }

  private def get(hub: SessionHub, req: Request): ZIO[Any, Nothing, Response] =
    val sid    = req.headers.get(SessionHeader)
    val accept = req.headers.get("Accept")
    (sid, accept) match
      case (None, _) =>
        ZIO.succeed(reply(Message.Error(None, RpcError.InvalidRequest("GET /mcp needs a session id"))))
      case (Some(id), Some(value)) if value.contains(MediaType.EventStream.render) =>
        hub.contains(id).flatMap {
          case false => ZIO.succeed(reply(Message.Error(None, RpcError.InvalidRequest("unknown session"))))
          case true  =>
            ZIO.succeed(
              Sse.response(
                // Firefox does not resolve fetch until the first body byte.
                zio.stream.ZStream.unwrapScoped {
                  hub
                    .attach(id)
                    .foldZIO(
                      _ => ZIO.succeed(zio.stream.ZStream.empty),
                      queue =>
                        ZIO.succeed(
                          zio.stream.ZStream.succeed(ServerSentEvent.Heartbeat) ++
                            zio.stream.ZStream.fromQueue(queue).map(message => ServerSentEvent(message.json.toJson))
                        ),
                    )
                }
              )
            )
        }
      case _ =>
        ZIO.succeed(reply(Message.Error(None, RpcError.InvalidRequest("GET /mcp needs Accept: text/event-stream"))))
    end match
  end get

  private def delete(hub: SessionHub, req: Request): ZIO[Any, Nothing, Response] =
    req.headers
      .get(SessionHeader)
      .fold(ZIO.succeed(Response.empty(Status.Ok)))(id => hub.close(id).as(Response.empty(Status.Ok)))

  private def post[R](engine: Engine[R], hub: SessionHub, req: Request): ZIO[R, Nothing, Response] =
    req.body.utf8.orElseSucceed("").flatMap { raw =>
      raw.fromJson[Json] match
        case Left(_)     => ZIO.succeed(reply(Message.Error(None, RpcError.ParseError("Parse error"))))
        case Right(json) =>
          Message.decode(json) match
            case Left(err)                       => ZIO.succeed(reply(err))
            case Right(msg) if session(req, msg) => sessionPost(engine, hub, req, msg)
            case Right(msg)                      =>
              mismatch(req.headers, msg) match
                case Some(err) => ZIO.succeed(reply(err))
                case None      => dispatch(engine, hub, req, msg, Era.Stateless, None)
    }

  private def session(req: Request, msg: Message): Boolean =
    method(msg).contains(Methods.Initialize) || req.headers.get(ProtocolHeader).contains(ProtocolVersion.Legacy.value)

  private def sessionPost[R](
      engine: Engine[R],
      hub: SessionHub,
      req: Request,
      msg: Message,
  ): ZIO[R, Nothing, Response] =
    val opening = method(msg).contains(Methods.Initialize)
    if opening then
      heddle.internal.Ids.uuid.flatMap { raw =>
        val id = raw.toString
        hub.open(id) *> engine.respond(msg, req.headers, Era.Session, Some(id)).map { out =>
          accepted(out).withHeader(Envelope.SessionHeader, id)
        }
      }
    else
      req.headers.get(Envelope.SessionHeader) match
        case None     => ZIO.succeed(reply(Message.Error(None, RpcError.InvalidRequest("missing session id"))))
        case Some(id) =>
          hub.contains(id).flatMap {
            case false => ZIO.succeed(reply(Message.Error(None, RpcError.InvalidRequest("unknown session"))))
            case true  =>
              dispatch(engine, hub, req, msg, Era.Session, Some(id)).map(_.withHeader(Envelope.SessionHeader, id))
          }
    end if
  end sessionPost

  /** `Accept` chooses the body before the tool runs. A cancel or a closed stream interrupts the tracked fiber. */
  private def dispatch[R](
      engine: Engine[R],
      hub: SessionHub,
      req: Request,
      msg: Message,
      era: Era,
      session: Option[String],
  ): ZIO[R, Nothing, Response] =
    msg match
      case request: Message.Request if acceptsEvents(req) => streamed(engine, hub, req, request, era, session)
      case request: Message.Request                       => joined(engine, hub, req, request, era, session)
      case _ => engine.respond(msg, req.headers, era, session).map(accepted)

  private def acceptsEvents(req: Request): Boolean =
    req.headers.get("Accept").exists(_.toLowerCase.contains(MediaType.EventStream.render))

  /** One JSON body. `publish` is unbound, so it sends nothing. The fiber is still tracked, so another POST can cancel
    * it.
    */
  private def joined[R](
      engine: Engine[R],
      hub: SessionHub,
      req: Request,
      msg: Message.Request,
      era: Era,
      session: Option[String],
  ): ZIO[R, Nothing, Response] =
    forkTracked(hub, session, msg.id, engine.respond(msg, req.headers, era, session), None).flatMap { fiber =>
      fiber.await.flatMap {
        case Exit.Success(out)   => ZIO.succeed(accepted(out))
        case Exit.Failure(cause) =>
          if cause.isInterrupted then ZIO.succeed(accepted(None)) else ZIO.failCause(cause)
      }
    }

  /** Notifications on this POST, then the result. The fiber is a child of the stream scope: when the pull stops, it is
    * interrupted. `send` finishes when the event is queued, not when the client reads it.
    */
  private def streamed[R](
      engine: Engine[R],
      hub: SessionHub,
      req: Request,
      msg: Message.Request,
      era: Era,
      session: Option[String],
  ): ZIO[R, Nothing, Response] =
    ZIO.environmentWith[R] { env =>
      val events =
        ZStream.unwrapScoped {
          ZIO.scopeWith { scope =>
            for
              queue <- Queue.unbounded[Option[ServerSentEvent]]
              send   = (note: Message.Notification) => queue.offer(Some(ServerSentEvent(note.json.toJson))).unit
              effect =
                Session
                  .bind(send)(engine.respond(msg, req.headers, era, session).provideEnvironment(env))
                  .flatMap { out =>
                    ZIO.foreachDiscard(out) { message =>
                      queue.offer(Some(ServerSentEvent(message.json.toJson)))
                    } *> queue.offer(None).unit
                  }
                  .onInterrupt(queue.offer(None).unit)
              _ <- forkTracked(hub, session, msg.id, effect, Some(scope))
            yield ZStream.fromQueue(queue).collectWhileSome
          }
        }
      Sse.response(events)
    }

  /** Fork `effect` and register it before `release`, so the body cannot run untracked. `scope` interrupts it on close.
    */
  private def forkTracked[R, A](
      hub: SessionHub,
      session: Option[String],
      id: RequestId,
      effect: ZIO[R, Nothing, A],
      scope: Option[Scope],
  ): ZIO[R, Nothing, Fiber[Nothing, A]] =
    for
      release <- Promise.make[Nothing, Unit]
      tracked <- Ref.make[Option[Fiber[Nothing, A]]](None)
      wrapped = (release.await *> effect).ensuring {
        tracked.get.flatMap {
          case Some(fiber) => hub.untrack(session, id, fiber)
          case None        => ZIO.unit
        }
      }
      fiber <- scope match
        case Some(live) => wrapped.forkIn(live)
        case None       => wrapped.fork
      _ <- tracked.set(Some(fiber))
      _ <- hub.track(session, id, fiber)
      _ <- release.succeed(())
    yield fiber

  private def method(msg: Message): Option[String] =
    msg match
      case Message.Request(_, m, _)   => Some(m)
      case Message.Notification(m, _) => Some(m)
      case _                          => None

  /** The headers must repeat what the body says, so a proxy can route and authorize without parsing JSON. */
  private def mismatch(headers: Headers, msg: Message): Option[Message] =
    msg match
      case Message.Request(id, m, params) =>
        val bodyVer = RequestMeta.of(params).protocolVersion.map(_.value)
        val headVer = headers.get(ProtocolHeader)
        val name    = params.get("name").collect { case Json.Str(n) => n }
        val problem =
          if headVer.isEmpty then Some(RpcError.HeaderMismatch(s"missing $ProtocolHeader header"))
          else if bodyVer.isEmpty then Some(RpcError.InvalidParams("missing protocol version"))
          else if headVer != bodyVer then Some(RpcError.HeaderMismatch(s"Header mismatch: $ProtocolHeader"))
          else if !headers.get(MethodHeader).contains(m) then
            Some(RpcError.HeaderMismatch(s"Header mismatch: $MethodHeader"))
          else if m == Methods.CallTool && (name.isEmpty || headers.get(NameHeader) != name) then
            Some(RpcError.HeaderMismatch(s"Header mismatch: $NameHeader"))
          else None
        problem.map(e => Message.Error(Some(id), e))
      case _ => None

  private def accepted(out: Option[Message]): Response =
    out.fold(Response.empty(Status.Accepted))(reply)

  private[transport] def reply(msg: Message): Response =
    Response.json(msg.json.toJson, status(msg))

  private def status(msg: Message): Status =
    msg match
      case Message.Error(_, e) =>
        e match
          case _: RpcError.MethodNotFound => Status.NotFound
          case _: RpcError.ParseError | _: RpcError.InvalidRequest | _: RpcError.InvalidParams |
              _: RpcError.HeaderMismatch | _: RpcError.UnsupportedVersion =>
            Status.BadRequest
          case _ => Status.Ok
      case _ => Status.Ok
end Http
