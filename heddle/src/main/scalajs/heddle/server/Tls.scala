package heddle.server

import heddle.error.{HttpError, TlsError}
import heddle.internal.Pem
import heddle.internal.duplex.{ByteConn, NodeConn}
import heddle.internal.node.{NodeTlsSocket, TlsOptions}
import scala.scalajs.js
import zio.*

final class Tls private (private[heddle] val certPem: String, private[heddle] val keyPem: String):
  private[heddle] def listener(
      config: heddle.Server.Config
  ): IO[heddle.error.ServerError, heddle.internal.duplex.Listener] =
    heddle.internal.duplex.NodeListener.tls(config, certPem, keyPem)

  private[heddle] def server(conn: ByteConn, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    conn match
      case n: NodeConn =>
        ZIO.async[Any, HttpError, Tls.Session] { cb =>
          val opts = TlsOptions(
            key = keyPem,
            cert = certPem,
            isServer = true,
            ALPNProtocols = js.Array(alpn.toList*),
          )
          val tlsSock = NodeTlsSocket(n.socket, opts)
          tlsSock.once(
            "secure",
            (() =>
              val proto = tlsSock.alpnProtocol match
                case s: String => s
                case _         => ""
              cb(ZIO.succeed(Tls.Session(NodeConn.tls(tlsSock), proto)))
            ): js.Function0[Unit],
          )
          tlsSock.once(
            "error",
            (
                (err: js.Error) =>
                  cb(ZIO.fail(HttpError.Io(java.io.IOException(Option(err.message).getOrElse("tls handshake")))))
            ): js.Function1[js.Error, Unit],
          )
          ()
        }
      case _ =>
        ZIO.fail(HttpError.Io(IllegalArgumentException("TLS server needs a Node connection")))
end Tls

object Tls:
  private[heddle] final class Session(val conn: ByteConn, val applicationProtocol: String)

  /** Node parses the PEM at handshake time, so the blocks are checked here to fail at startup instead. */
  def pem(certPem: String, keyPem: String): ZLayer[Any, TlsError, Tls] =
    ZLayer.fromZIO(
      (ZIO.fromEither(Pem.body(certPem, "CERTIFICATE")) *> ZIO.fromEither(Pem.body(keyPem, "PRIVATE KEY")))
        .as(Tls(certPem, keyPem))
    )
