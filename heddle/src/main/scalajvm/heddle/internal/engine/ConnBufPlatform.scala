package heddle.internal.engine

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import heddle.error.HttpError
import heddle.server.Tls
import zio.*

private[heddle] object ConnBufPlatform:
  def soTimeout(sock: java.net.Socket, d: Duration): Unit =
    val ms =
      if d == Duration.Infinity || d.toNanos <= 0L then 0
      else math.max(1L, d.toMillis).min(Int.MaxValue.toLong).toInt
    try sock.setSoTimeout(ms)
    catch case _: Throwable => ()

  def channel(buf: ByteBuffer, ch: SocketChannel): ConnBuf =
    buf.limit(0)
    val in = ch.socket.getInputStream
    ConnBuf(
      buf,
      dest =>
        ZIO
          .attemptBlockingInterrupt {
            val n = in.read(dest.array(), dest.arrayOffset() + dest.position(), dest.remaining())
            if n > 0 then dest.position(dest.position() + n)
            n
          }
          .mapError(HttpError.Io(_)),
      d => soTimeout(ch.socket, d),
    )
  end channel

  def inputStream(buf: ByteBuffer, in: java.io.InputStream, sock: java.net.Socket): ConnBuf =
    ConnBuf.fromPull(buf, Tls.pull(in, math.max(1024, buf.capacity.min(16 * 1024))), d => soTimeout(sock, d))
end ConnBufPlatform
