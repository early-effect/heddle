package heddle.mcp.transport

import heddle.http.{Method, Request, Response, Status}
import heddle.http.header.Headers
import heddle.mcp.protocol.{Era, Message, Methods, ProtocolVersion, RequestMeta, RpcError}
import heddle.mcp.server.{Engine, Envelope}
import heddle.route.{Handler, Routes}
import zio.json.*
import zio.json.ast.Json
import zio.{ZIO, ZNothing}

/** Streamable HTTP. 2026-07-28 requests carry their method, tool name, and version in headers too, and the two must
  * agree. A request that says `initialize`, or names 2025-11-25, is a session request.
  */
object Http:
  val ProtocolHeader = "MCP-Protocol-Version"
  val MethodHeader   = "Mcp-Method"
  val NameHeader     = "Mcp-Name"
  val SessionHeader  = Envelope.SessionHeader

  def routes[R](engine: Engine[R], path: String = "mcp"): Routes[R, ZNothing] =
    val segs = path.split('/').filter(_.nonEmpty).toList
    Routes.fromHandler(Handler { (req: Request) =>
      if req.path.segments.toList != segs then ZIO.succeed(Response.notFound())
      else
        req.method match
          case Method.POST   => post(engine, req)
          case Method.DELETE => ZIO.succeed(Response.empty(Status.Ok))
          case _             => ZIO.succeed(Response.methodNotAllowed("POST, DELETE"))
    })
  end routes

  private def post[R](engine: Engine[R], req: Request): ZIO[R, Nothing, Response] =
    req.body.utf8.orElseSucceed("").flatMap { raw =>
      raw.fromJson[Json] match
        case Left(_)     => ZIO.succeed(reply(Message.Error(None, RpcError.ParseError("Parse error"))))
        case Right(json) =>
          Message.decode(json) match
            case Left(err)                       => ZIO.succeed(reply(err))
            case Right(msg) if session(req, msg) => sessionPost(engine, req, msg)
            case Right(msg)                      =>
              mismatch(req.headers, msg) match
                case Some(err) => ZIO.succeed(reply(err))
                case None      => engine.respond(msg, req.headers, Era.Stateless).map(accepted(_))
    }

  private def session(req: Request, msg: Message): Boolean =
    method(msg).contains(Methods.Initialize) || req.headers.get(ProtocolHeader).contains(ProtocolVersion.Legacy.value)

  private def sessionPost[R](engine: Engine[R], req: Request, msg: Message): ZIO[R, Nothing, Response] =
    val opening = method(msg).contains(Methods.Initialize)
    val sid     =
      if opening then heddle.internal.Ids.uuid.map(id => Some(id.toString))
      else ZIO.succeed(req.headers.get(Envelope.SessionHeader))
    (sid <*> engine.respond(msg, req.headers, Era.Session)).map { (id, out) =>
      val res = accepted(out)
      id.fold(res)(res.withHeader(Envelope.SessionHeader, _))
    }

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
