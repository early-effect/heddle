package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
trait ProcessStream extends js.Object:
  def on(event: String, listener: js.Function): this.type = js.native
  def write(buffer: Uint8Array): Boolean                  = js.native
  def pause(): this.type                                  = js.native
  def resume(): this.type                                 = js.native
end ProcessStream

@js.native
trait ProcessEnv extends js.Object:
  @js.annotation.JSBracketAccess
  def apply(key: String): js.UndefOr[String] = js.native

  @js.annotation.JSBracketAccess
  def update(key: String, value: String): Unit = js.native
end ProcessEnv

@js.native
@JSImport("node:process", JSImport.Namespace)
private[heddle] object Process extends js.Object:
  val stdin: ProcessStream  = js.native
  val stdout: ProcessStream = js.native
  val env: ProcessEnv       = js.native
end Process
