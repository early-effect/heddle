package heddle.internal.node

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

class TlsOptions(
    var key: js.UndefOr[String] = js.undefined,
    var cert: js.UndefOr[String] = js.undefined,
    var ALPNProtocols: js.UndefOr[js.Array[String]] = js.undefined,
    var isServer: js.UndefOr[Boolean] = js.undefined,
    var servername: js.UndefOr[String] = js.undefined,
    var rejectUnauthorized: js.UndefOr[Boolean] = js.undefined,
    var host: js.UndefOr[String] = js.undefined,
    var port: js.UndefOr[Int] = js.undefined,
) extends js.Object

@js.native
trait TlsSocket extends NetSocket:
  val alpnProtocol: String | Boolean | Null = js.native

@js.native
@JSImport("node:tls", JSImport.Namespace)
private[heddle] object NodeTls extends js.Object:
  def createServer(
      options: TlsOptions,
      connectionListener: js.Function1[TlsSocket, Unit],
  ): NetServer = js.native

  def connect(options: TlsOptions, cb: js.Function0[Unit]): TlsSocket = js.native
end NodeTls

@js.native
@JSImport("node:tls", "TLSSocket")
private[heddle] class NodeTlsSocket(
    @unused socket: NetSocket,
    @unused options: TlsOptions,
) extends TlsSocket
