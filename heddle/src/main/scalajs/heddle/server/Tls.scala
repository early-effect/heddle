package heddle.server

import heddle.error.TlsError
import heddle.internal.Pem
import heddle.internal.duplex.{ByteConn, NodeConn}
import zio.*

final class Tls private (private[heddle] val certPem: String, private[heddle] val keyPem: String):
  private[heddle] def listener(
      config: heddle.Server.Config
  ): IO[heddle.error.ServerError, heddle.internal.duplex.Listener[NodeConn]] =
    heddle.internal.duplex.NodeListener.tls(config, certPem, keyPem)

object Tls:
  private[heddle] final class Session(val conn: ByteConn, val applicationProtocol: String)

  /** Node parses the PEM at handshake time, so the blocks are checked here to fail at startup instead. */
  def pem(certPem: String, keyPem: String): ZLayer[Any, TlsError, Tls] =
    ZLayer.fromZIO(
      (ZIO.fromEither(Pem.body(certPem, "CERTIFICATE")) *> ZIO.fromEither(Pem.body(keyPem, "PRIVATE KEY")))
        .as(Tls(certPem, keyPem))
    )
end Tls
