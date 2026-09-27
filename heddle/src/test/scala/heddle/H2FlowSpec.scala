package heddle

import heddle.internal.h2.H2Flow
import zio.*
import zio.test.*

object H2FlowSpec extends ZIOSpecDefault:
  private def suspended(f: Fiber.Runtime[?, ?]): UIO[Unit] =
    f.status.repeatUntil(_.isInstanceOf[Fiber.Status.Suspended]).unit

  /** A connection whose peer advertised `initial` as its stream window. */
  private def flow(initial: Long): UIO[H2Flow] = H2Flow.make(65535).tap(_.resizeSend(initial))

  def spec = suite("H2Flow")(
    test("received data past the stream window is refused"):
      H2Flow.make(16).flatMap(f => f.open(1) *> f.takeRecv(1, 17)).map(ok => assertTrue(!ok))
    ,
    test("a sender waits for credit, and a WINDOW_UPDATE releases it"):
      for
        f      <- flow(8)
        _      <- f.open(1)
        first  <- f.takeSend(1, 8)
        waiter <- f.takeSend(1, 1).fork
        _      <- suspended(waiter)
        _      <- f.creditSend(1, 1)
        got    <- waiter.join
      yield assertTrue(first.contains(8), got.contains(1))
    ,
    test("credit for one blocked stream wakes that stream, whoever else is waiting"):
      for
        f    <- flow(0)
        _    <- f.open(1) *> f.open(3)
        a    <- f.takeSend(1, 10).fork
        _    <- suspended(a)
        b    <- f.takeSend(3, 10).fork
        _    <- suspended(b)
        _    <- f.creditSend(3, 10)
        done <- b.join.timeout(2.seconds)
        _    <- a.interrupt
      yield assertTrue(done.flatten.contains(10))
    ,
    test("a window smaller than the frame still moves data, up to what it allows"):
      flow(5).flatMap(f => f.open(1) *> f.takeSend(1, 10)).map(got => assertTrue(got.contains(5)))
    ,
    test("a new SETTINGS_INITIAL_WINDOW_SIZE moves every open stream's window by the difference"):
      for
        f     <- flow(100)
        _     <- f.open(1)
        all   <- f.takeSend(1, 100)
        _     <- f.resizeSend(150)
        extra <- f.takeSend(1, 100)
      yield assertTrue(all.contains(100), extra.contains(50))
    ,
    test("a window pushed past 2^31-1 is refused, and a closed stream's sender gets None"):
      for
        f      <- flow(0)
        _      <- f.open(1)
        over   <- f.creditSend(1, Int.MaxValue) <*> f.creditSend(1, 1)
        _      <- f.resizeSend(0)
        _      <- f.open(3)
        waiter <- f.takeSend(3, 1).fork
        _      <- suspended(waiter)
        _      <- f.close(3)
        orphan <- waiter.join
      yield assertTrue(over == (true, false), orphan.isEmpty),
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
end H2FlowSpec
