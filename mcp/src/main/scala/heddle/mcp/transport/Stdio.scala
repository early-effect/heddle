package heddle.mcp.transport

import heddle.LinePipe
import heddle.http.header.Headers
import heddle.mcp.protocol.{Era, Message, Methods, ProtocolVersion, RequestMeta, RpcError}
import heddle.mcp.server.{Engine, SessionHub}
import zio.json.*
import zio.json.ast.Json
import zio.{Semaphore, ZIO}

/** Newline-delimited JSON-RPC on a pipe. `initialize` switches the pipe to a 2025-11-25 session; a request that names
  * 2026-07-28, or `server/discover`, is answered statelessly either way.
  */
object Stdio:
  /** One pipe is one session. Subscribe and server notifications share it with replies. */
  val SessionId = "stdio"

  def run[R](engine: Engine[R], hub: SessionHub, pipe: LinePipe): ZIO[R, Throwable, Unit] =
    Semaphore.make(1).flatMap { writes =>
      def emit(msg: Message): ZIO[R, Throwable, Unit] =
        writes.withPermit(pipe.writeLine(msg.json.toJson))
      def loop(era: Era): ZIO[R, Throwable, Unit] =
        pipe.readLine.flatMap {
          case None       => ZIO.unit
          case Some(line) =>
            dispatch(engine, hub, line, era).flatMap { (next, reply) =>
              reply.fold(loop(next))(msg => emit(msg) *> loop(next))
            }
        }
      hub.open(SessionId) *>
        hub
          .events(SessionId)
          .catchAll(_ => zio.stream.ZStream.empty)
          .foreach(emit)
          .fork
          .flatMap(ear => loop(Era.Stateless).ensuring(ear.interrupt))
    }
  end run

  private def dispatch[R](
      engine: Engine[R],
      hub: SessionHub,
      line: String,
      era: Era,
  ): ZIO[R, Nothing, (Era, Option[Message])] =
    if line.isBlank then ZIO.succeed((era, None))
    else
      line
        .fromJson[Json]
        .left
        .map(_ => Message.Error(None, RpcError.ParseError("Parse error")))
        .flatMap(Message.decode) match
        case Left(err)  => ZIO.succeed((era, Some(err)))
        case Right(msg) =>
          val next  = eraOf(msg, era)
          val ready = if next == Era.Session then hub.open(SessionId) else ZIO.unit
          val sid   = if next == Era.Session then Some(SessionId) else None
          ready *> engine.respond(msg, Headers.empty, next, sid).map(out => (next, out))

  private def eraOf(msg: Message, current: Era): Era =
    msg match
      case Message.Request(_, Methods.Initialize, _) => Era.Session
      case Message.Request(_, Methods.Discover, _)   => Era.Stateless
      case Message.Request(_, _, p) if RequestMeta.of(p).protocolVersion.contains(ProtocolVersion.Current) =>
        Era.Stateless
      case _ => current
end Stdio
