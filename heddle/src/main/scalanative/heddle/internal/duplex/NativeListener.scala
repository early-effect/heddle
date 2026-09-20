package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.posix.{AsyncFd, Net}
import zio.*

private[heddle] final class NativeListener(
    listenFd: Int,
    tcpNoDelay: Boolean,
    soKeepAlive: Boolean,
    inbound: Queue[Either[Throwable, Int]],
) extends Listener:
  @volatile private var closed = false

  def localPort: UIO[Int] = ZIO.succeed(Net.localPort(listenFd))

  def acceptFd: Task[Int] =
    inbound.take.flatMap {
      case Left(e)   => ZIO.fail(e)
      case Right(fd) => ZIO.succeed(fd)
    }

  def accept: Task[ByteConn] =
    acceptFd.map(NativeConn.of)

  def close: UIO[Unit] =
    ZIO.succeed {
      if !closed then
        closed = true
        Net.close(listenFd)
    } *> inbound.shutdown

  private[duplex] def produce: UIO[Unit] =
    def loop: UIO[Unit] =
      ZIO.suspendSucceed {
        if closed then ZIO.unit
        else
          AsyncFd
            .readable(listenFd)
            .foldZIO(
              e =>
                val err =
                  if closed || Option(e.getMessage).contains("listener closed") then
                    java.io.IOException("listener closed")
                  else e
                inbound.offer(Left(err)).unit
              ,
              _ =>
                if closed then ZIO.unit
                else
                  ZIO.attempt(Net.accept(listenFd)).orElseSucceed(-1).flatMap { fd =>
                    if fd < 0 then loop
                    else
                      try
                        Net.setTcpNoDelay(fd, tcpNoDelay)
                        Net.setKeepAlive(fd, soKeepAlive)
                      catch case _: Throwable => ()
                      inbound.offer(Right(fd)) *> loop
                  },
            )
      }
    loop
  end produce
end NativeListener

object NativeListener:
  def bind(config: Server.Config): ZIO[Scope, ServerError, NativeListener] =
    for
      inbound  <- Queue.unbounded[Either[Throwable, Int]]
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
