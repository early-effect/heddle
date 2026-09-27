package heddle

import heddle.error.{HttpError, ServerError, TlsError}
import heddle.internal.duplex.{AcceptError, ByteConn, NativeListener}
import heddle.server.Tls
import zio.*

private object NativeTls:
  /** What `Server.install` does with a `Tls` layer: accept a plain connection, then handshake with `Tls.server`. */
  final class Listener(plain: NativeListener, tls: Tls):
    def localPort: UIO[Int] = plain.localPort

    def accept: IO[AcceptError | HttpError, ByteConn] =
      plain.accept.flatMap(tls.server(_, Chunk.empty)).map(_.conn)

  def listener(config: Server.Config): ZIO[Scope, ServerError | TlsError, Listener] =
    for
      tls   <- ZIO.service[Tls].provideLayer(Tls.pem(TestTls.certPem, TestTls.keyPem))
      plain <- NativeListener.bind(config)
    yield Listener(plain, tls)

end NativeTls
