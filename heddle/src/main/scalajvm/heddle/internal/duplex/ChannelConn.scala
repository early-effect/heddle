package heddle.internal.duplex

import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
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

  def closeNow(): Unit =
    try ch.close()
    catch case _: Throwable => ()

  def close: UIO[Unit] = ZIO.succeed(closeNow())

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

  def closeNow(): Unit =
    try ssl.close()
    catch case _: Throwable => ()

  def close: UIO[Unit] = ZIO.succeed(closeNow())

  def setReadTimeout(d: Duration): UIO[Unit] =
    ZIO.succeed(ConnBufPlatform.soTimeout(ssl, d))
end SslConn

private[heddle] final class ChannelListener(ss: ServerSocketChannel, tcpNoDelay: Boolean, soKeepAlive: Boolean)
    extends Listener:
  def localPort: UIO[Int] = ZIO.succeed(Nio.localPort(ss))

  def accept: Task[ByteConn] =
    Nio.accept(ss).flatMap { ch =>
      ZIO.attempt(ch.setOption(StandardSocketOptions.TCP_NODELAY, tcpNoDelay)).ignore *>
        ZIO.attempt(ch.setOption(StandardSocketOptions.SO_KEEPALIVE, soKeepAlive)).ignore *>
        ZIO.succeed(ChannelConn(ch))
    }

  def close: UIO[Unit] =
    ZIO.succeed {
      try ss.close()
      catch case _: Throwable => ()
    }
end ChannelListener

private[heddle] object ChannelListener:
  def bind(config: Server.Config): IO[ServerError, Listener] =
    Nio.openServer(config).map(ss => ChannelListener(ss, config.tcpNoDelay, config.soKeepAlive))
