package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("node:crypto", JSImport.Namespace)
private[heddle] object SecureRandom extends js.Object:
  def randomFillSync(buf: Uint8Array): Uint8Array = js.native
