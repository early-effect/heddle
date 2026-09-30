package heddle.internal

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8Array

/** Web Crypto's CSPRNG, the one browsers and Node (20 and later) both give `globalThis`, so the same ids come from a
  * server in a page as from one on Node.
  */
@js.native
@JSGlobal("crypto")
private object WebCrypto extends js.Object:
  def getRandomValues(buf: Uint8Array): Uint8Array = js.native

private[heddle] object IdsPlatform:
  /** The most bytes Web Crypto fills in one call. */
  private val chunk = 65536

  def fill(dst: Array[Byte]): Boolean =
    var offset = 0
    while offset < dst.length do
      val buf = new Uint8Array(math.min(chunk, dst.length - offset))
      val _   = WebCrypto.getRandomValues(buf)
      var i   = 0
      while i < buf.length do
        dst(offset + i) = buf(i).toByte
        i += 1
      offset += buf.length
    true
  end fill
end IdsPlatform
