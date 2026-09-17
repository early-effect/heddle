package heddle.mcp.transport

import heddle.http.{Request, Response, Status}
import heddle.mcp.protocol.{Engine, Legacy}
import heddle.mcp.protocol.JsonRpc.*
import zio.json.ast.Json
import zio.ZIO

import java.util.UUID

object LegacyHttp:
  def post[R](engine: Engine[R], req: Request, msg: Json.Obj): ZIO[R, Nothing, Response] =
    methodOf(msg) match
      case Some("initialize") =>
        val body =
          Legacy.initializeResult(parseId(msg), engine.serverName, engine.serverVersion, engine.instructions)
        val sid = UUID.randomUUID().toString
        ZIO.succeed(Http.rpcResponse(Status.Ok, body).withHeader(Legacy.SessionHeader, sid))
      case Some(method) if method.startsWith("notifications/") =>
        ZIO.succeed(echoSession(req, Response.empty(Status.Accepted)))
      case _ =>
        engine.handleCompat(msg, req.headers).map {
          case None      => echoSession(req, Response.empty(Status.Accepted))
          case Some(out) => echoSession(req, Http.rpcResponse(Http.httpStatus(out), Legacy.stripEnvelope(out)))
        }

  private def echoSession(req: Request, res: Response): Response =
    req.headers.get(Legacy.SessionHeader).fold(res)(id => res.withHeader(Legacy.SessionHeader, id))
end LegacyHttp
