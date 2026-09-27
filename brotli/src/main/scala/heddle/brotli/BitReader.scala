package heddle.brotli

/** LSB-first reader, inverse of [[BitWriter]]. The first failure sticks; after it every read is zero bits, so a decode
  * loop checks `ok` between steps rather than unwinding from each read.
  */
private[brotli] final class BitReader(data: Array[Byte]):
  private var i      = 0
  private var acc    = 0L
  private var nbits  = 0
  private var eofPad = 0
  private var first  = Option.empty[BrotliError]

  def failure: Option[BrotliError] = first
  def ok: Boolean                  = first.isEmpty

  def fail(e: BrotliError): Unit =
    if first.isEmpty then first = Some(e)

  private def fill(need: Int): Unit =
    while nbits < need && nbits <= 56 do
      if i < data.length then
        acc |= (data(i) & 0xffL) << nbits
        i += 1
      else if eofPad < 8 then eofPad += 1
      else fail(BrotliError.Truncated)
      nbits += 8

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
    if pad != 0 && readBits(pad) != 0 then fail(BrotliError.CorruptPadding)

  /** `len` whole bytes, checked against what is left before anything is allocated. */
  def copyBytes(len: Int): Array[Byte] =
    val buffered = nbits / 8
    if (nbits & 7) != 0 then
      fail(BrotliError.Unaligned)
      Array.emptyByteArray
    else if len.toLong > buffered + (data.length - i).toLong then
      fail(BrotliError.Truncated)
      Array.emptyByteArray
    else
      val out = Array.ofDim[Byte](len)
      var o   = 0
      while nbits >= 8 && o < len do
        out(o) = (acc & 0xff).toByte
        acc >>>= 8
        nbits -= 8
        o += 1
      val need = len - o
      if need > 0 then
        System.arraycopy(data, i, out, o, need)
        i += need
      out
    end if
  end copyBytes
end BitReader
