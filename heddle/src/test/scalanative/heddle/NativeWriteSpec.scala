package heddle

import heddle.internal.duplex.NativeConn
import heddle.internal.posix.{AsyncFd, Net}
import java.nio.ByteBuffer
import zio.*
import zio.test.*

object NativeWriteSpec extends ZIOSpecDefault:
  private val size = 8 * 1024 * 1024

  def spec = suite("Native writes")(
    test("a write larger than the socket buffer finishes while the peer only reads") {
      ZIO.scoped {
        for
          lfd <- ZIO.acquireRelease(ZIO.fromEither(Net.listen("127.0.0.1", 0, 16, reuse = true)))(fd =>
            ZIO.succeed(Net.close(fd))
          )
          port   <- ZIO.succeed(Net.localPort(lfd))
          cfd    <- ZIO.fromEither(Net.connect("127.0.0.1", port))
          _      <- AsyncFd.writable(cfd)
          _      <- AsyncFd.readable(lfd)
          sfd    <- ZIO.fromEither(Net.accept(lfd)).some
          server <- ZIO.acquireRelease(ZIO.succeed(NativeConn.of(sfd)))(_.close)
          client <- ZIO.acquireRelease(ZIO.succeed(NativeConn.of(cfd)))(_.close)
          seen   <- Ref.make(0)
          reader <- drain(client, seen).fork
          wrote  <- server.write(Chunk.fill(size)(7.toByte)).timeout(5.seconds)
          _      <- reader.join.timeout(5.seconds)
          got    <- seen.get
        yield assertTrue(wrote.isDefined, got == size)
      }
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)

  private def drain(conn: NativeConn, seen: Ref[Int]): IO[HttpError, Unit] =
    val buf                      = ByteBuffer.allocate(64 * 1024)
    val step: IO[HttpError, Int] =
      ZIO.succeed(buf.clear()) *> conn.read(buf).tap(n => seen.update(_ + n).when(n > 0))
    step.repeatUntilZIO(n => seen.get.map(total => n < 0 || total >= size)).unit
end NativeWriteSpec
