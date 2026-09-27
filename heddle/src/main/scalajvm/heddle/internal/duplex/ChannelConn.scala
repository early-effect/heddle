package heddle.internal.duplex

import java.nio.ByteBuffer
import java.nio.channels.{ClosedChannelException, ServerSocketChannel, SocketChannel}
import java.net.StandardSocketOptions
import heddle.Server
import heddle.error.{HttpError, ServerError}
import heddle.internal.engine.{ConnBufPlatform, Nio}
import zio.*

private[heddle] final class ChannelConn(val ch: SocketChannel) extends ByteConn:
  private val scratch = ByteBuffer.allocate(8192)

  def read(dst: ByteBuffer): IO[HttpError, Int] =
    val in = ch.socket.getInputStream
    ZIO
      .attemptBlockingInterrupt {
        val n = in.read(dst.array(), dst.arrayOffset() + dst.position(), dst.remaining())
        if n > 0 then dst.position(dst.position() + n)
        n
      }
      .mapError(HttpError.Io(_))

  def write(chunk: Chunk[Byte]): Task[Unit] =
    Nio.writeChunk(ch, chunk, scratch)

  def close: UIO[Unit] = ZIO.attempt(ch.close()).ignore

  def setReadTimeout(d: Duration): UIO[Unit] =
    ZIO.succeed(ConnBufPlatform.soTimeout(ch.socket, d))
end ChannelConn

private[heddle] final class SslConn(ssl: javax.net.ssl.SSLSocket) extends ByteConn:
  def read(dst: ByteBuffer): IO[HttpError, Int] =
    val in = ssl.getInputStream
    ZIO
      .attemptBlockingInterrupt {
        val n = in.read(dst.array(), dst.arrayOffset() + dst.position(), dst.remaining())
        if n > 0 then dst.position(dst.position() + n)
        n
      }
      .mapError(HttpError.Io(_))

  def write(chunk: Chunk[Byte]): Task[Unit] =
    heddle.server.Tls.writer(ssl.getOutputStream)(chunk)

  def close: UIO[Unit] = ZIO.attempt(ssl.close()).ignore

  def setReadTimeout(d: Duration): UIO[Unit] =
    ZIO.succeed(ConnBufPlatform.soTimeout(ssl, d))
end SslConn

private[heddle] final class ChannelListener(ss: ServerSocketChannel, tcpNoDelay: Boolean, soKeepAlive: Boolean)
    extends Listener:
  def localPort: UIO[Int] = ZIO.succeed(Nio.localPort(ss))

  def accept: IO[AcceptError, ByteConn] =
    Nio
      .accept(ss)
      .mapError {
        case _: ClosedChannelException => AcceptError.Closed
        case other                     => AcceptError.Failed(other)
      }
      .flatMap { ch =>
        ZIO.attempt(ch.setOption(StandardSocketOptions.TCP_NODELAY, tcpNoDelay)).ignore *>
          ZIO.attempt(ch.setOption(StandardSocketOptions.SO_KEEPALIVE, soKeepAlive)).ignore *>
          ZIO.succeed(ChannelConn(ch))
      }

  def close: UIO[Unit] = ZIO.attempt(ss.close()).ignore
end ChannelListener

private[heddle] object ChannelListener:
  def bind(config: Server.Config): IO[ServerError, Listener] =
    Nio.openServer(config).map(ss => ChannelListener(ss, config.tcpNoDelay, config.soKeepAlive))
