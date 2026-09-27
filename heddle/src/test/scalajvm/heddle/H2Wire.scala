package heddle

import heddle.error.WireError
import heddle.internal.h2.{FrameCodec, H2Frame}
import java.net.Socket
import zio.*

/** A raw HTTP/2 client for tests: typed frames over a socket, no exceptions for a peer that closes. */
object H2Wire:
  final class Wire(socket: Socket, buffer: Ref[Chunk[Byte]], seen: Ref[Chunk[H2Frame]]):
    def send(frames: H2Frame*): Task[Unit] =
      ZIO.attemptBlocking {
        socket.getOutputStream.write(Chunk.fromIterable(frames).flatMap(FrameCodec.encode).toArray)
        socket.getOutputStream.flush()
      }

    /** Every frame the server has sent once `done` holds for them, acknowledging its SETTINGS on the way. */
    def until(done: Chunk[H2Frame] => Boolean): Task[Chunk[H2Frame]] =
      seen.get.flatMap { frames =>
        if done(frames) then ZIO.succeed(frames)
        else
          buffer.get.flatMap { buf =>
            FrameCodec.decode(buf, 1 << 24) match
              case Right((frame, rest)) =>
                val ack = frame match
                  case H2Frame.Settings(_) => send(H2Frame.SettingsAck)
                  case _                   => ZIO.unit
                buffer.set(rest) *> seen.update(_ :+ frame) *> ack *> until(done)
              case Left(WireError.TruncatedFrame) =>
                ZIO
                  .attemptBlocking {
                    val tmp = new Array[Byte](65536)
                    val n   = socket.getInputStream.read(tmp)
                    Chunk.fromArray(tmp.take(n.max(0))) -> (n < 0)
                  }
                  .flatMap { (more, eof) =>
                    if eof then ZIO.succeed(frames) else buffer.update(_ ++ more) *> until(done)
                  }
              case Left(e) => ZIO.fail(java.io.IOException(e.message))
          }
      }
  end Wire

  def apply[A](port: Int, settings: Chunk[(Int, Int)] = Chunk.empty)(f: Wire => Task[A]): Task[A] =
    ZIO.scoped {
      for
        socket <- ZIO.fromAutoCloseable(ZIO.attemptBlocking(Socket("127.0.0.1", port)))
        _      <- ZIO.attemptBlocking(socket.setSoTimeout(5000))
        w      <- (Ref.make(Chunk.empty[Byte]) <*> Ref.make(Chunk.empty[H2Frame])).map(Wire(socket, _, _))
        _      <- ZIO.attemptBlocking(socket.getOutputStream.write(H2Frame.Preface))
        _      <- w.send(H2Frame.Settings(settings))
        a      <- f(w)
      yield a
    }
end H2Wire
