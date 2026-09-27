package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.posix.{AsyncFd, Net, NetError, Syscall}
import scala.scalanative.posix.errno.{EMFILE, ENFILE, ENOBUFS, ENOMEM}
import java.util.concurrent.atomic.AtomicBoolean
import zio.*

/** A listening socket whose `produce` fiber turns readiness into accepted descriptors on `inbound`. */
private[heddle] final class NativeListener(
    listenFd: Int,
    tcpNoDelay: Boolean,
    soKeepAlive: Boolean,
    inbound: Queue[Either[AcceptError, Int]],
) extends Listener:
  private val closed = AtomicBoolean(false)

  def localPort: UIO[Int] = ZIO.succeed(Net.localPort(listenFd))

  def acceptFd: IO[AcceptError, Int] =
    inbound.take.flatMap(ZIO.fromEither(_))

  def accept: IO[AcceptError, ByteConn] =
    acceptFd.map(NativeConn.of)

  def close: UIO[Unit] =
    ZIO.succeed(if closed.compareAndSet(false, true) then Net.close(listenFd)) *> inbound.shutdown

  /** Tuning is best effort: a socket that refuses an option still serves. */
  private def configure(fd: Int): UIO[Unit] =
    ZIO.succeed(Net.setTcpNoDelay(fd, tcpNoDelay)) *> ZIO.succeed(Net.setKeepAlive(fd, soKeepAlive)).unit

  private[duplex] def produce: UIO[Unit] =
    ZIO.suspendSucceed {
      if closed.get() then ZIO.unit
      else
        AsyncFd
          .readable(listenFd)
          .foldZIO(
            e => inbound.offer(Left(if closed.get() then AcceptError.Closed else AcceptError.Failed(e.exception))).unit,
            _ =>
              if closed.get() then ZIO.unit
              else
                ZIO.succeed(Net.accept(listenFd)).flatMap {
                  case Right(Some(fd)) => configure(fd) *> inbound.offer(Right(fd)) *> produce
                  case Right(None)     => produce
                  // Out of descriptors or memory, the connection stays queued and poll says readable at once: back off.
                  case Left(e @ NetError.Failed(Syscall.Accept, errno)) if exhausted(errno) =>
                    ZIO.logWarning(s"${e.message}; accepting again in 100ms") *> ZIO.sleep(100.millis) *> produce
                  // Anything else (the peer reset before we took it, say) costs that connection only.
                  case Left(_) => produce
                },
          )
    }

  private def exhausted(errno: Int): Boolean =
    errno == EMFILE || errno == ENFILE || errno == ENOBUFS || errno == ENOMEM
end NativeListener

object NativeListener:
  def bind(config: Server.Config): ZIO[Scope, ServerError, NativeListener] =
    for
      inbound  <- Queue.unbounded[Either[AcceptError, Int]]
      listener <- ZIO
        .suspendSucceed(ZIO.fromEither(Net.listen(config.host, config.port, config.soBacklog, config.reuseAddress)))
        .mapBoth(
          e => ServerError.BindFailed(config.host, config.port, e.exception),
          NativeListener(_, config.tcpNoDelay, config.soKeepAlive, inbound),
        )
      fiber <- listener.produce.fork
      // Close the listen fd first so a blocked accept returns, then interrupt.
      _ <- ZIO.addFinalizer(listener.close *> fiber.interrupt)
    yield listener
end NativeListener
