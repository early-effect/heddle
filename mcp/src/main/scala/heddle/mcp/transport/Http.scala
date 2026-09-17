package heddle.mcp.transport

import heddle.http.{Method, Request, Response, Status}
import heddle.http.header.Headers
import heddle.mcp.protocol.{Engine, Legacy}
import heddle.mcp.protocol.JsonRpc.*
import heddle.route.{Handler, Routes}
import zio.json.EncoderOps
import zio.json.DecoderOps
import zio.json.ast.Json
import zio.{ZIO, ZNothing}

object Http:
  val ProtocolHeader = "MCP-Protocol-Version"
  val MethodHeader   = "Mcp-Method"
  val NameHeader     = "Mcp-Name"

  def routes[R](engine: Engine[R], path: String = "mcp"): Routes[R, ZNothing] =
    val segs = path.split('/').filter(_.nonEmpty).toList
    Routes.fromHandler(Handler { (req: Request) =>
      if !matches(req, segs) then ZIO.succeed(Response.notFound())
      else
        req.method match
          case Method.POST   => post(engine, req)
          case Method.DELETE => ZIO.succeed(Response.empty(Status.Ok))
          case Method.GET    => ZIO.succeed(Response.methodNotAllowed("POST, DELETE"))
          case _             => ZIO.succeed(Response.methodNotAllowed("POST, DELETE"))
    })
  end routes

  private def matches(req: Request, segs: List[String]): Boolean =
    req.path.segments.toList == segs

  private def post[R](engine: Engine[R], req: Request): ZIO[R, Nothing, Response] =
    req.body.utf8.orElseSucceed("").flatMap { raw =>
      raw.fromJson[Json] match
        case Left(_) =>
          ZIO.succeed(rpcResponse(Status.BadRequest, error(Json.Null, ParseError, "Parse error")))
        case Right(json) =>
          json match
            case msg: Json.Obj if isLegacy(req.headers, msg) =>
              LegacyHttp.post(engine, req, msg)
            case msg: Json.Obj =>
              validateHeaders(req.headers, msg) match
                case Some(err) => ZIO.succeed(rpcResponse(Status.BadRequest, err))
                case None      =>
                  engine
                    .handle(msg, req.headers)
                    .map:
                      case None      => Response.empty(Status.Accepted)
                      case Some(out) => rpcResponse(httpStatus(out), out)
            case _ =>
              ZIO.succeed(rpcResponse(Status.BadRequest, error(Json.Null, ParseError, "Parse error")))
    }

  private def isLegacy(headers: Headers, msg: Json.Obj): Boolean =
    val method  = methodOf(msg)
    val headVer = headers.get(ProtocolHeader)
    method.contains("initialize") || headVer.contains(Legacy.ProtocolVersion)

  private def validateHeaders(headers: Headers, msg: Json.Obj): Option[Json.Obj] =
    val id         = parseId(msg)
    val method     = methodOf(msg)
    val params     = paramsOf(msg)
    val bodyVer    = protocolVersion(params)
    val headVer    = headers.get(ProtocolHeader)
    val headMethod = headers.get(MethodHeader)
    val headName   = headers.get(NameHeader)
    val bodyName   = params
      .get("name")
      .flatMap:
        case Json.Str(n) => Some(n)
        case _           => None
    if headVer.isEmpty then Some(error(id, HeaderMismatch, "missing MCP-Protocol-Version header"))
    else if bodyVer.isEmpty then Some(error(id, InvalidParams, "missing protocol version"))
    else if !headVer.contains(bodyVer.get) then Some(error(id, HeaderMismatch, s"Header mismatch: $ProtocolHeader"))
    else if method.exists(m => !headMethod.contains(m)) then
      Some(error(id, HeaderMismatch, s"Header mismatch: $MethodHeader"))
    else if method.contains("tools/call") && (headName.isEmpty || headName != bodyName) then
      Some(error(id, HeaderMismatch, s"Header mismatch: $NameHeader"))
    else None
  end validateHeaders

  private[transport] def httpStatus(rpc: Json.Obj): Status =
    rpc
      .get("error")
      .flatMap:
        case o: Json.Obj =>
          o.get("code")
            .flatMap:
              case Json.Num(n) =>
                n.intValue match
                  case ParseError | InvalidRequest | HeaderMismatch | UnsupportedVersion | InvalidParams =>
                    Some(Status.BadRequest)
                  case MethodNotFound => Some(Status.NotFound)
                  case _              => Some(Status.Ok)
              case _ => None
        case _ => None
      .getOrElse(Status.Ok)

  private[transport] def rpcResponse(status: Status, body: Json.Obj): Response =
    Response.json(body.toJson, status)
end Http
