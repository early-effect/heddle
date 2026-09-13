package heddle.brotli

/** LSB-first reader, inverse of [[BitWriter]]. */
private[brotli] final class BitReader(data: Array[Byte]):
  private var i      = 0
  private var acc    = 0L
  private var nbits  = 0
  private var eofPad = 0

  private def fill(need: Int): Unit =
    while nbits < need && nbits <= 56 do
      if i < data.length then
        acc |= (data(i) & 0xffL) << nbits
        i += 1
        nbits += 8
      else if eofPad < 8 then
        nbits += 8
        eofPad += 1
      else throw BrotliException("truncated bitstream")

  def readBits(n: Int): Int =
    if n <= 0 then 0
    else
      fill(n)
      val v = (acc & ((1L << n) - 1)).toInt
      acc >>>= n
      nbits -= n
      v

  def peekBits(n: Int): Int =
    fill(n)
    (acc & ((1L << n) - 1)).toInt

  def dropBits(n: Int): Unit =
    fill(n)
    acc >>>= n
    nbits -= n

  def jumpToByteBoundary(): Unit =
    val pad = nbits & 7
    if pad != 0 && readBits(pad) != 0 then throw BrotliException("corrupted padding bits")

  def copyBytes(len: Int): Array[Byte] =
    if (nbits & 7) != 0 then throw BrotliException("unaligned copyBytes")
    val out = Array.ofDim[Byte](len)
    var o   = 0
    while nbits >= 8 && o < len do
      out(o) = (acc & 0xff).toByte
      acc >>>= 8
      nbits -= 8
      o += 1
    val need = len - o
    if i + need > data.length then throw BrotliException("truncated bitstream")
    if need > 0 then
      System.arraycopy(data, i, out, o, need)
      i += need
    out
  end copyBytes
end BitReader
