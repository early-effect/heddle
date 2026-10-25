package heddle.mcp.transport

import heddle.LinePipe
import heddle.http.header.Headers
import heddle.mcp.Session
import heddle.mcp.protocol.{Era, Message, Methods, ProtocolVersion, RequestId, RequestMeta, RpcError}
import heddle.mcp.server.{Engine, SessionHub}
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, Exit, Fiber, Promise, Ref, Semaphore, UIO, ZIO}

/** Newline-delimited JSON-RPC on one pipe. `initialize` is answered before the next line is read. Every other request
  * runs on its own fiber, so the next line can be read while a tool is still running, and that tool can publish a
  * notification ahead of its result. One permit covers every write. A 2026-07-28 request, or `server/discover`, is
  * answered statelessly; `initialize` switches the pipe to a 2025-11-25 session.
  */
object Stdio:
  /** One pipe is one session. Subscribe and server notifications share it with replies. */
  val SessionId = "stdio"

  def run[R](engine: Engine[R], hub: SessionHub, pipe: LinePipe): ZIO[R, Throwable, Unit] =
    ZIO.scoped {
      for
        writes  <- Semaphore.make(1)
        running <- Ref.make(Chunk.empty[Fiber[Throwable, Unit]])
        _       <- hub.open(SessionId)
        _       <- serve(engine, hub, pipe, writes, running)
      yield ()
    }

  private def serve[R](
      engine: Engine[R],
      hub: SessionHub,
      pipe: LinePipe,
      writes: Semaphore,
      running: Ref[Chunk[Fiber[Throwable, Unit]]],
  ): ZIO[R & zio.Scope, Throwable, Unit] =
    def emit(msg: Message): ZIO[Any, Throwable, Unit] =
      writes.withPermit(pipe.writeLine(msg.json.toJson))

    // A failed write interrupts this request so its result is not written after a notification that never landed.
    def send(note: Message.Notification): UIO[Unit] =
      emit(note).catchAll(_ => ZIO.interrupt *> ZIO.unit)

    def write(out: Option[Message]): ZIO[Any, Throwable, Unit] =
      out match
        case None      => ZIO.unit
        case Some(msg) => emit(msg)

    // The pipe is the one session opened above. Era still chooses the envelope; the id is how cancel finds the fiber.
    def answer(msg: Message, era: Era): ZIO[R, Nothing, Option[Message]] =
      engine.respond(msg, Headers.empty, era, Some(SessionId))

    def release(id: RequestId, self: Promise[Nothing, Fiber[Throwable, Unit]]): UIO[Unit] =
      self.await.flatMap { fiber =>
        hub.untrack(Some(SessionId), id, fiber) *>
          running.update(_.filterNot(_ == fiber))
      }

    // Track before the body runs. The section is uninterruptible so close cannot observe a half-registered fiber.
    // `forkScoped` would otherwise keep that uninterruptible flag, and cancel could not stop the tool.
    def forkRequest(id: RequestId, effect: ZIO[R, Throwable, Unit]): ZIO[R & zio.Scope, Nothing, Unit] =
      ZIO.uninterruptible {
        for
          gate <- Promise.make[Nothing, Unit]
          self <- Promise.make[Nothing, Fiber[Throwable, Unit]]
          body = (gate.await *> effect).interruptible.ensuring(release(id, self))
          fiber <- body.forkScoped
          _     <- self.succeed(fiber)
          _     <- hub.track(Some(SessionId), id, fiber)
          _     <- running.update(_ :+ fiber)
          _     <- gate.succeed(())
        yield ()
      }

    def route(msg: Message, era: Era): ZIO[R & zio.Scope, Throwable, Unit] =
      msg match
        case Message.Request(_, Methods.Initialize, _) =>
          answer(msg, era).flatMap(write)
        case Message.Request(id, _, _) =>
          forkRequest(id, Session.bind(send)(answer(msg, era)).flatMap(write))
        case _ =>
          answer(msg, era).flatMap(write)

    def drain: ZIO[Any, Throwable, Unit] =
      running.get.flatMap { fibers =>
        ZIO.foreachDiscard(fibers) { fiber =>
          fiber.await.flatMap {
            case Exit.Success(_)                                => ZIO.unit
            case Exit.Failure(cause) if cause.isInterruptedOnly => ZIO.unit
            case Exit.Failure(cause)                            => ZIO.failCause(cause)
          }
        }
      }

    def loop(era: Era): ZIO[R & zio.Scope, Throwable, Unit] =
      pipe.readLine.flatMap {
        case None       => drain
        case Some(line) =>
          if line.isBlank then loop(era)
          else
            decode(line) match
              case Left(err)  => emit(err) *> loop(era)
              case Right(msg) =>
                val next   = eraOf(msg, era)
                val opened = if next == Era.Session then hub.open(SessionId) else ZIO.unit
                opened *> route(msg, next) *> loop(next)
      }

    hub.events(SessionId).catchAll(_ => ZStream.empty).foreach(emit).forkScoped *> loop(Era.Stateless)
  end serve

  private def decode(line: String): Either[Message, Message] =
    line.fromJson[Json].left.map(_ => Message.Error(None, RpcError.ParseError("Parse error"))).flatMap(Message.decode)

  private def eraOf(msg: Message, current: Era): Era =
    msg match
      case Message.Request(_, Methods.Initialize, _) => Era.Session
      case Message.Request(_, Methods.Discover, _)   => Era.Stateless
      case Message.Request(_, _, p) if RequestMeta.of(p).protocolVersion.contains(ProtocolVersion.Current) =>
        Era.Stateless
      case _ => current
end Stdio
