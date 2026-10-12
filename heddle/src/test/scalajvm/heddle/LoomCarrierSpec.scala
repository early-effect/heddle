package heddle

import java.net.{ServerSocket, Socket, SocketTimeoutException}
import java.util.concurrent.atomic.AtomicBoolean
import heddle.internal.engine.Carrier
import zio.*
import zio.test.*

/** The keep-alive fast path trusts three facts. Prove them before leaning on them: the loom executor runs a forked
  * fiber on a virtual thread, `SO_TIMEOUT` fails a read on that thread, and closing the socket unblocks it. `Clock`
  * identity decides whether the loop may skip its ZIO timeout race.
  */
object LoomCarrierSpec extends ZIOSpecDefault:
  def spec =
    val clock = suite("clock identity")(
      test("withLiveClock is Clock.ClockLive") {
        ZIO.clock.map(clock => assertTrue(clock eq Clock.ClockLive))
      } @@ TestAspect.withLiveClock,
      test("the suite clock is not Clock.ClockLive") {
        ZIO.clock.map(clock => assertTrue(clock ne Clock.ClockLive))
      },
    )

    val carrier = suite("virtual thread")(
      test("a forked fiber runs on a virtual thread") {
        ZIO.attempt(Thread.currentThread.isVirtual).fork.flatMap(_.join).map(virtual => assertTrue(virtual))
      },
      test("SO_TIMEOUT fails a read on that thread") {
        ZIO.scoped {
          for
            listener <- ZIO.fromAutoCloseable(ZIO.attempt(ServerSocket(0)))
            accepted <- ZIO.attemptBlocking(listener.accept()).fork
            client   <- ZIO.fromAutoCloseable(ZIO.attempt(Socket("127.0.0.1", listener.getLocalPort)))
            peer     <- accepted.join
            _        <- ZIO.addFinalizer(ZIO.attempt(peer.close()).ignore)
            onVirtual = AtomicBoolean(false)
            failed <- Carrier
              .inline {
                onVirtual.set(Thread.currentThread.isVirtual)
                client.setSoTimeout(50)
                client.getInputStream.read()
              }
              .either
              .fork
              .flatMap(_.join)
          yield assertTrue(onVirtual.get(), failed.left.exists(_.isInstanceOf[SocketTimeoutException]))
        }
      },
      test("closing the socket unblocks a read on that thread") {
        ZIO.scoped {
          for
            listener <- ZIO.fromAutoCloseable(ZIO.attempt(ServerSocket(0)))
            accepted <- ZIO.attemptBlocking(listener.accept()).fork
            client   <- ZIO.fromAutoCloseable(ZIO.attempt(Socket("127.0.0.1", listener.getLocalPort)))
            peer     <- accepted.join
            _        <- ZIO.addFinalizer(ZIO.attempt(peer.close()).ignore)
            blocked  <- Carrier.inline(client.getInputStream.read()).either.fork
            _        <- ZIO.attempt(client.close())
            failed   <- blocked.join
          yield assertTrue(failed.isLeft)
        }
      },
    ).provideLayer(Runtime.enableLoomBasedExecutor) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds)

    suite("loom carrier")(clock, carrier)
  end spec
end LoomCarrierSpec
