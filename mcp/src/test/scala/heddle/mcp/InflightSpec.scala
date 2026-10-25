package heddle.mcp

import heddle.mcp.protocol.*
import heddle.mcp.server.{Engine, Offer, SessionHub}
import zio.*
import zio.json.ast.Json
import zio.test.*

object InflightSpec extends ZIOSpecDefault:
  private val note: Message.Notification =
    Message.Notification("notifications/progress", Json.Obj("progress" -> Json.Num(1)))

  private def waiting: ZIO[Any, Nothing, (Fiber[Nothing, Unit], Promise[Nothing, Unit], Promise[Nothing, Unit])] =
    for
      started     <- Promise.make[Nothing, Unit]
      interrupted <- Promise.make[Nothing, Unit]
      gate        <- Promise.make[Nothing, Unit]
      fiber       <- (started.succeed(()) *> gate.await).onInterrupt(interrupted.succeed(())).fork
      _           <- started.await
    yield (fiber, gate, interrupted)

  private def engine(hub: SessionHub): Engine[Any] =
    Engine(Implementation("spec", "0"), Offer(Chunk.empty, Chunk.empty, Map.empty), None, 0L, hub)

  def spec = suite("in-flight requests")(
    test("publish reaches only the request it was bound to"):
      for
        seen <- Ref.make(Chunk.empty[String])
        send = (n: Message.Notification) => seen.update(_ :+ n.method)
        _ <- Session.publish(note)
        _ <- Session.bind(send) {
          Session.publish(note) *> Session.publish(note).fork.flatMap(_.join)
        }
        got <- seen.get
      yield assertTrue(got == Chunk(note.method, note.method))
    ,
    test("untrack of a replaced fiber leaves the newer one, which interrupt then stops"):
      for
        hub                          <- ZIO.succeed(new SessionHub)
        _                            <- hub.open("s")
        (fiber, _, interrupted)      <- waiting
        (newer, _, newerInterrupted) <- waiting
        _                            <- hub.track(Some("s"), RequestId.Num(1), fiber)
        _                            <- hub.track(Some("s"), RequestId.Num(1), newer)
        _                            <- hub.untrack(Some("s"), RequestId.Num(1), fiber)
        _                            <- hub.interrupt(Some("s"), RequestId.Num(1))
        _                            <- newerInterrupted.await
        first                        <- interrupted.poll
      yield assertTrue(first.isEmpty)
    ,
    test("a cancel for another session does not interrupt"):
      for
        hub                     <- ZIO.succeed(new SessionHub)
        _                       <- hub.open("s")
        _                       <- hub.open("other")
        (fiber, _, interrupted) <- waiting
        _                       <- hub.track(Some("s"), RequestId.Str("a"), fiber)
        _                       <- hub.interrupt(Some("other"), RequestId.Str("a"))
        _                       <- hub.interrupt(None, RequestId.Str("a"))
        state                   <- interrupted.poll
      yield assertTrue(state.isEmpty)
    ,
    test("closing the session interrupts its fibers"):
      for
        hub                     <- ZIO.succeed(new SessionHub)
        _                       <- hub.open("s")
        (fiber, _, interrupted) <- waiting
        (loose, _, looseStop)   <- waiting
        _                       <- hub.track(Some("s"), RequestId.Num(3), fiber)
        _                       <- hub.track(None, RequestId.Num(3), loose)
        _                       <- hub.close("s")
        _                       <- interrupted.await
        looseState              <- looseStop.poll
      yield assertTrue(looseState.isEmpty)
    ,
    test("the engine turns notifications/cancelled into that interrupt"):
      for
        hub                     <- ZIO.succeed(new SessionHub)
        _                       <- hub.open("s")
        (fiber, _, interrupted) <- waiting
        _                       <- hub.track(Some("s"), RequestId.Num(9), fiber)
        gone                    <- engine(hub).respond(
          Cancelled(RequestId.Num(9), Some("stop")).message,
          heddle.http.header.Headers.empty,
          Era.Session,
          Some("s"),
        )
        _ <- interrupted.await
      yield assertTrue(gone.isEmpty)
    ,
    test("a malformed cancel and an unknown id leave the fiber running"):
      for
        hub                     <- ZIO.succeed(new SessionHub)
        _                       <- hub.open("s")
        (fiber, _, interrupted) <- waiting
        _                       <- hub.track(Some("s"), RequestId.Num(4), fiber)
        _                       <- engine(hub).respond(
          Message.Notification(Notifications.Cancelled, Json.Obj("requestId" -> Json.Bool(true))),
          heddle.http.header.Headers.empty,
          Era.Session,
          Some("s"),
        )
        _ <- engine(hub).respond(
          Cancelled(RequestId.Num(99), None).message,
          heddle.http.header.Headers.empty,
          Era.Stateless,
          None,
        )
        state <- interrupted.poll
      yield assertTrue(state.isEmpty),
  )
end InflightSpec
