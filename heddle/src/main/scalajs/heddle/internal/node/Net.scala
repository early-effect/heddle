package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
trait AddressInfo extends js.Object:
  val port: Int       = js.native
  val family: String  = js.native
  val address: String = js.native

@js.native
trait NetSocket extends js.Object:
  def on(event: String, listener: js.Function): this.type                  = js.native
  def once(event: String, listener: js.Function): this.type                = js.native
  def write(buffer: Uint8Array, cb: js.Function1[js.Error, Unit]): Boolean = js.native
  def pause(): this.type                                                   = js.native
  def resume(): this.type                                                  = js.native
  def destroy(): this.type                                                 = js.native
  def setTimeout(ms: Int): this.type                                       = js.native
  val destroyed: Boolean                                                   = js.native
end NetSocket

@js.native
trait NetServer extends js.Object:
  def on(event: String, listener: js.Function): this.type                = js.native
  def once(event: String, listener: js.Function): this.type              = js.native
  def listen(port: Int, host: String, cb: js.Function0[Unit]): this.type = js.native
  def close(cb: js.Function0[Unit]): this.type                           = js.native
  def address(): AddressInfo | String | Null                             = js.native
  val listening: Boolean                                                 = js.native
end NetServer

@js.native
@JSImport("node:net", JSImport.Namespace)
private[heddle] object Net extends js.Object:
  def createServer(): NetServer                                                  = js.native
  def createServer(connectionListener: js.Function1[NetSocket, Unit]): NetServer = js.native
