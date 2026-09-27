package heddle

import heddle.internal.duplex.{NativeConn, TlsListener}
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{AsyncFd, Net, SslIo}
import scala.scalanative.posix.fcntl
import zio.*
import zio.test.*

object NativeTlsCloseSpec extends ZIOSpecDefault:
  private val local: Server.Config =
    Server.Config.default.copy(host = "127.0.0.1", port = 0, http2 = false)

  private def open(fd: Int): Boolean = fcntl.fcntl(fd, fcntl.F_GETFD, 0) >= 0

  def spec = suite("Native TLS close")(
    test("closing a TLS connection releases its socket") {
      ZIO.scoped {
        for
          listener <- TlsListener.bind(local, NativeTls.certPem, NativeTls.keyPem)
          port     <- listener.localPort
          accepted <- listener.accept.fork
          fd       <- ZIO.attempt(Net.connect("127.0.0.1", port))
          _        <- AsyncFd.writable(fd)
          session  <- ZIO.attempt(Ssl.connect(Ssl.clientCtx(Some(NativeTls.certPem)), fd, "localhost"))
          _        <- SslIo.handshake(session, accept = false, fd)
          conn = NativeConn.tls(fd, session)
          before <- ZIO.succeed(open(fd))
          _      <- conn.close
          after  <- ZIO.succeed(open(fd))
          _      <- accepted.interrupt
        yield assertTrue(before, !after)
      }
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds)
end NativeTlsCloseSpec
