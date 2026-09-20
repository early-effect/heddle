package heddle.internal

import heddle.internal.node.SecureRandom
import scala.scalajs.js.typedarray.Uint8Array

private[heddle] object IdsPlatform:
  def fill(dst: Array[Byte]): Unit =
    val buf = new Uint8Array(dst.length)
    val _   = SecureRandom.randomFillSync(buf)
    var i   = 0
    while i < dst.length do
      dst(i) = buf(i).toByte
      i += 1
end IdsPlatform
