package heddle.route

import heddle.http.{Method, Request, Response, Status}
import heddle.http.header.{HeaderName, Headers}
import heddle.server.Files
import zio.*

final case class Handler[-R, +E](run: Request => ZIO[R, E, Response]):
  def apply(request: Request): ZIO[R, E, Response] = run(request)

  def mapError[E2](f: E => E2): Handler[R, E2] =
    Handler(req => run(req).mapError(f))

  def catchAll[R1 <: R, E2](f: E => ZIO[R1, E2, Response]): Handler[R1, E2] =
    Handler(req => run(req).catchAll(f))

  def orElse[R1 <: R, E1 >: E](that: Handler[R1, E1]): Handler[R1, E1] =
    Handler(req => run(req).orElse(that.run(req)))
end Handler

object Handler:
  def fromFunctionZIO[R, E](f: Request => ZIO[R, E, Response]): Handler[R, E] = Handler(f)

  def fromZIO[R, E](effect: ZIO[R, E, Response]): Handler[R, E] = Handler(_ => effect)

  def succeed(response: Response): Handler[Any, Nothing] = Handler(_ => ZIO.succeed(response))

  def text(value: String): Handler[Any, Nothing] = succeed(Response.text(value))

  def html(value: String): Handler[Any, Nothing] = succeed(Response.html(value))

  def json(value: String): Handler[Any, Nothing] = succeed(Response.json(value))

  val ok: Handler[Any, Nothing] = succeed(Response.ok)

  val notFound: Handler[Any, Nothing] = succeed(Response.notFound())

  def fromFile(file: java.io.File): Handler[Any, Throwable] =
    Handler(_ => Files.fromPath(file.toPath))

  def websocket[R](run: heddle.ws.WebSocket => ZIO[R, Throwable, Unit]): Handler[R, Nothing] =
    Handler { req =>
      handshake(req) match
        case Left(res) => ZIO.succeed(res)
        case Right(hs) =>
          ZIO.environmentWith[R] { env =>
            Response.withWs(
              hs.status,
              hs.headers,
              (src, send) =>
                val sock = heddle.ws.LiveWebSocket(src, send)
                run(sock).provideEnvironment(env),
            )
          }
    }

  private final case class WsHandshake(status: Status, headers: Headers)

  private def handshake(req: Request): Either[Response, WsHandshake] =
    if req.header(HeaderName(":protocol")).exists(_.equalsIgnoreCase("websocket")) then
      Right(WsHandshake(Status.Ok, Headers.empty))
    else
      val upgrade = req.header(HeaderName.Upgrade).exists(_.equalsIgnoreCase("websocket"))
      val conn    = req.header(HeaderName.Connection).exists(_.toLowerCase.contains("upgrade"))
      val key     = req.header(HeaderName.SecWebSocketKey)
      if req.method != Method.GET then Left(Response.methodNotAllowed("GET"))
      else if !upgrade || !conn || key.isEmpty then Left(Response.badRequest("Expected WebSocket upgrade"))
      else
        Right(
          WsHandshake(
            Status.SwitchingProtocols,
            Headers.empty
              .add(HeaderName.Upgrade, "websocket")
              .add(HeaderName.Connection, "Upgrade")
              .add(HeaderName.SecWebSocketAccept, heddle.ws.WsCodec.acceptKey(key.get)),
          )
        )
end Handler

def handler[R, E](f: Request => ZIO[R, E, Response]): Handler[R, E] = Handler(f)

def handler[R, E](effect: ZIO[R, E, Response]): Handler[R, E] = Handler(_ => effect)

def handler(response: Response): Handler[Any, Nothing] = Handler.succeed(response)
