package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.posix.{AsyncFd, Net}
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

  private def configure(fd: Int): UIO[Unit] =
    ZIO.attempt(Net.setTcpNoDelay(fd, tcpNoDelay)).ignore *> ZIO.attempt(Net.setKeepAlive(fd, soKeepAlive)).ignore

  private[duplex] def produce: UIO[Unit] =
    ZIO.suspendSucceed {
      if closed.get() then ZIO.unit
      else
        AsyncFd
          .readable(listenFd)
          .foldZIO(
            cause => inbound.offer(Left(if closed.get() then AcceptError.Closed else AcceptError.Failed(cause))).unit,
            _ =>
              if closed.get() then ZIO.unit
              else
                ZIO.attempt(Net.accept(listenFd)).option.flatMap {
                  case Some(fd) if fd >= 0 => configure(fd) *> inbound.offer(Right(fd)) *> produce
                  case _                   => produce
                },
          )
    }
end NativeListener

object NativeListener:
  def bind(config: Server.Config): ZIO[Scope, ServerError, NativeListener] =
    for
      inbound  <- Queue.unbounded[Either[AcceptError, Int]]
      listener <- ZIO
        .attempt {
          val fd = Net.listen(config.host, config.port, config.soBacklog, config.reuseAddress)
          NativeListener(fd, config.tcpNoDelay, config.soKeepAlive, inbound)
        }
        .mapError(e => ServerError.BindFailed(config.host, config.port, e))
      fiber <- listener.produce.fork
      // Close the listen fd first so a blocked accept returns, then interrupt.
      _ <- ZIO.addFinalizer(listener.close *> fiber.interrupt)
    yield listener
end NativeListener
