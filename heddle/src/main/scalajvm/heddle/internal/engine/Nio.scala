package heddle.internal.engine

import java.net.InetSocketAddress
import java.net.StandardSocketOptions
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import heddle.error.ServerError
import heddle.Server
import zio.*

private[heddle] object Nio:
  def openServer(config: Server.Config): IO[ServerError, ServerSocketChannel] =
    ZIO
      .attempt {
        val ch = ServerSocketChannel.open()
        ch.setOption(StandardSocketOptions.SO_REUSEADDR, config.reuseAddress)
        ch.bind(InetSocketAddress(config.host, config.port), config.soBacklog)
        ch
      }
      .mapError(ServerError.BindFailed(config.host, config.port, _))

  def localPort(ch: ServerSocketChannel): Int =
    ch.getLocalAddress match
      case a: InetSocketAddress => a.getPort
      case _                    => 0

  def accept(ch: ServerSocketChannel): Task[SocketChannel] =
    ZIO.attempt(ch.accept())

  /** `Chunk.fromArray` wraps the array (`ByteArray`). Wrap that same array in a ByteBuffer so the write syscall is
    * zero-copy. Other chunk shapes copy into the reused scratch buffer.
    */
  def writeChunk(ch: SocketChannel, chunk: Chunk[Byte], scratch: ByteBuffer): Task[Unit] =
    if chunk.isEmpty then ZIO.unit
    else
      ZIO.attempt {
        val buf = chunk match
          case Chunk.ByteArray(array, offset, length) =>
            ByteBuffer.wrap(array, offset, length)
          case _ =>
            val n = chunk.length
            if n <= scratch.capacity then
              chunk.copyToArray(scratch.array(), 0, n)
              scratch.position(0)
              scratch.limit(n)
              scratch
            else ByteBuffer.wrap(chunk.toArray)
        while buf.hasRemaining do
          val w = ch.write(buf)
          if w < 0 then throw java.io.IOException("channel closed during write")
        ()
      }

  def writer(ch: SocketChannel, buf: ByteBuffer): Chunk[Byte] => Task[Unit] =
    chunk => writeChunk(ch, chunk, buf)
end Nio
