package heddle.internal.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("node:zlib", JSImport.Namespace)
private[heddle] object Zlib extends js.Object:
  def gzipSync(buf: Uint8Array): Uint8Array                         = js.native
  def gunzipSync(buf: Uint8Array): Uint8Array                       = js.native
  def gunzipSync(buf: Uint8Array, options: ZlibOptions): Uint8Array = js.native

/** `zlib` options. `maxOutputLength` makes inflation throw a `RangeError` instead of growing without bound. */
private[heddle] final class ZlibOptions(val maxOutputLength: Double) extends js.Object
