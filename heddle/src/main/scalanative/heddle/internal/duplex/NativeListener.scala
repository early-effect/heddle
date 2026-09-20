package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.posix.Net
import zio.*

private[heddle] final class NativeListener(listenFd: Int, tcpNoDelay: Boolean, soKeepAlive: Boolean) extends Listener:
  @volatile private var closed = false

  def localPort: UIO[Int] = ZIO.succeed(Net.localPort(listenFd))

  def acceptFd: Task[Int] =
    ZIO.attemptBlockingInterrupt {
      if closed then throw java.io.IOException("listener closed")
      val fd =
        try Net.accept(listenFd)
        catch
          case e: Throwable =>
            if closed then throw java.io.IOException("listener closed")
            else throw e
      try
        Net.setTcpNoDelay(fd, tcpNoDelay)
        Net.setKeepAlive(fd, soKeepAlive)
      catch case _: Throwable => ()
      fd
    }

  def accept: Task[ByteConn] =
    acceptFd.map(NativeConn.of)

  def close: UIO[Unit] =
    ZIO.succeed {
      if !closed then
        closed = true
        Net.close(listenFd)
    }
end NativeListener

object NativeListener:
  def bind(config: Server.Config): IO[ServerError, NativeListener] =
    ZIO
      .attempt {
        val fd = Net.listen(config.host, config.port, config.soBacklog, config.reuseAddress)
        NativeListener(fd, config.tcpNoDelay, config.soKeepAlive)
      }
      .mapError(e => ServerError.BindFailed(config.host, config.port, e))
end NativeListener
