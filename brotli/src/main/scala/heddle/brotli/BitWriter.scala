package heddle.brotli

/** LSB-first bit packer, RFC 7932. */
private[brotli] final class BitWriter:
  private val buf   = Array.newBuilder[Byte]
  private var cur   = 0
  private var nbits = 0

  /** Prefix codes are packed MSB first (RFC 7932 §1.5.1). */
  def writePrefix(nbits: Int, code: Int): Unit =
    var i = nbits - 1
    while i >= 0 do
      writeBits(1, (code >>> i) & 1)
      i -= 1

  def writeBits(n: Int, value: Long): Unit =
    var v    = value
    var left = n
    while left > 0 do
      val take = math.min(left, 8 - nbits)
      cur |= ((v & ((1L << take) - 1)).toInt) << nbits
      nbits += take
      v >>>= take
      left -= take
      if nbits == 8 then
        buf += cur.toByte
        cur = 0
        nbits = 0

  def alignByte(): Unit =
    if nbits > 0 then
      buf += cur.toByte
      cur = 0
      nbits = 0

  def writeBytes(data: Array[Byte]): Unit =
    alignByte()
    var i = 0
    while i < data.length do
      buf += data(i)
      i += 1

  def takeAligned(): Array[Byte] =
    alignByte()
    val a = buf.result()
    buf.clear()
    a

  /** Completed bytes only; keeps a partial last byte in `cur` for the next write. */
  def takeFullBytes(): Array[Byte] =
    val a = buf.result()
    buf.clear()
    a

  def finish(): Array[Byte] =
    alignByte()
    buf.result()
end BitWriter
