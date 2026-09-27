package heddle.mcp.transport

import heddle.LinePipe
import heddle.http.header.Headers
import heddle.mcp.protocol.{Era, Message, Methods, ProtocolVersion, RequestMeta, RpcError}
import heddle.mcp.server.Engine
import zio.json.*
import zio.json.ast.Json
import zio.ZIO

/** Newline-delimited JSON-RPC on a pipe. `initialize` switches the pipe to a 2025-11-25 session; a request that names
  * 2026-07-28, or `server/discover`, is answered statelessly either way.
  */
object Stdio:
  def run[R](engine: Engine[R], pipe: LinePipe): ZIO[R, Throwable, Unit] =
    def loop(era: Era): ZIO[R, Throwable, Unit] =
      pipe.readLine.flatMap {
        case None       => ZIO.unit
        case Some(line) =>
          dispatch(engine, line, era).flatMap { (next, reply) =>
            reply.fold(loop(next))(msg => pipe.writeLine(msg.json.toJson) *> loop(next))
          }
      }
    loop(Era.Stateless)
  end run

  private def dispatch[R](engine: Engine[R], line: String, era: Era): ZIO[R, Nothing, (Era, Option[Message])] =
    if line.isBlank then ZIO.succeed((era, None))
    else
      line
        .fromJson[Json]
        .left
        .map(_ => Message.Error(None, RpcError.ParseError("Parse error")))
        .flatMap(Message.decode) match
        case Left(err)  => ZIO.succeed((era, Some(err)))
        case Right(msg) =>
          val next = eraOf(msg, era)
          engine.respond(msg, Headers.empty, next).map(out => (next, out))

  private def eraOf(msg: Message, current: Era): Era =
    msg match
      case Message.Request(_, Methods.Initialize, _) => Era.Session
      case Message.Request(_, Methods.Discover, _)   => Era.Stateless
      case Message.Request(_, _, p) if RequestMeta.of(p).protocolVersion.contains(ProtocolVersion.Current) =>
        Era.Stateless
      case _ => current
end Stdio
