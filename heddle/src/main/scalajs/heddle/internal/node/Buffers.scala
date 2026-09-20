package heddle.internal.node

import scala.scalajs.js.typedarray.Uint8Array
import zio.Chunk

private[heddle] object Buffers:
  def toU8(bytes: Chunk[Byte]): Uint8Array =
    val a = Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do
      a(i) = (bytes(i) & 0xff).toShort
      i += 1
    a

  def fromU8(a: Uint8Array): Chunk[Byte] =
    val n   = a.length
    val out = Array.ofDim[Byte](n)
    var i   = 0
    while i < n do
      out(i) = a(i).toByte
      i += 1
    Chunk.fromArray(out)

  def copyU8(src: Uint8Array, dst: Array[Byte], off: Int, max: Int): Int =
    val n = math.min(max, src.length)
    var i = 0
    while i < n do
      dst(off + i) = src(i).toByte
      i += 1
    n
end Buffers
