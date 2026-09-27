package heddle

import heddle.internal.duplex.NativeConn
import heddle.internal.posix.{AsyncFd, Net}
import zio.*
import zio.test.*

object NativeSigpipeSpec extends ZIOSpecDefault:
  def spec = suite("Native SIGPIPE")(
    test("writing to a socket whose peer closed fails with an error and the process survives") {
      ZIO.scoped {
        for
          lfd <- ZIO.acquireRelease(ZIO.fromEither(Net.listen("127.0.0.1", 0, 16, reuse = true)))(fd =>
            ZIO.succeed(Net.close(fd))
          )
          port <- ZIO.succeed(Net.localPort(lfd))
          cfd  <- ZIO.fromEither(Net.connect("127.0.0.1", port))
          _    <- AsyncFd.writable(cfd)
          _    <- AsyncFd.readable(lfd)
          sfd  <- ZIO.fromEither(Net.accept(lfd)).some
          _    <- ZIO.succeed(Net.close(cfd))
          server = NativeConn.of(sfd)
          wrote <- server.write(Chunk.fill(4 * 1024 * 1024)(1.toByte)).repeatN(3).either
          _     <- server.close
        yield assertTrue(wrote.isLeft)
      }
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds)
end NativeSigpipeSpec
