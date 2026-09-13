package heddle.brotli

/** RFC 7932 dictionary word transforms. */
private[brotli] final case class WordTransform(prefix: Array[Byte], op: Int, suffix: Array[Byte])

private[brotli] object WordTransform:
  val Identity: Int       = 0
  val UppercaseFirst: Int = 10
  val UppercaseAll: Int   = 11

  val all: Array[WordTransform] =
    val in = getClass.getResourceAsStream("/heddle/brotli/transforms.txt")
    if in == null then throw IllegalStateException("missing resource heddle/brotli/transforms.txt")
    val text =
      try String(in.readAllBytes(), java.nio.charset.StandardCharsets.US_ASCII)
      finally in.close()
    text.split("\n").filter(_.nonEmpty).map { line =>
      val parts = line.split("\\|", -1)
      WordTransform(unhex(parts(0)), parts(1).toInt, unhex(parts(2)))
    }

  def omitFirst(op: Int): Int = if op >= 12 then op - 11 else 0
  def omitLast(op: Int): Int  = if op >= 1 && op <= 9 then op else 0

  def apply(dst: Array[Byte], dstOff: Int, word: Array[Byte], wordOff: Int, len0: Int, tx: WordTransform): Int =
    var offset = dstOff
    var i      = 0
    while i < tx.prefix.length do
      dst(offset) = tx.prefix(i)
      offset += 1
      i += 1
    val skip0 = omitFirst(tx.op)
    val skip  = if skip0 > len0 then len0 else skip0
    var woff  = wordOff + skip
    val len   = len0 - skip - omitLast(tx.op)
    i = len
    while i > 0 do
      dst(offset) = word(woff)
      offset += 1
      woff += 1
      i -= 1
    if tx.op == UppercaseAll || tx.op == UppercaseFirst then
      var up = offset - len
      var n  = if tx.op == UppercaseFirst then 1 else len
      while n > 0 do
        val tmp = dst(up) & 0xff
        if tmp < 0xc0 then
          if tmp >= 'a' && tmp <= 'z' then dst(up) = (dst(up) ^ 32).toByte
          up += 1
          n -= 1
        else if tmp < 0xe0 then
          dst(up + 1) = (dst(up + 1) ^ 32).toByte
          up += 2
          n -= 2
        else
          dst(up + 2) = (dst(up + 2) ^ 5).toByte
          up += 3
          n -= 3
    end if
    i = 0
    while i < tx.suffix.length do
      dst(offset) = tx.suffix(i)
      offset += 1
      i += 1
    offset - dstOff
  end apply

  private def unhex(s: String): Array[Byte] =
    if s.isEmpty then Array.emptyByteArray
    else
      val out = Array.ofDim[Byte](s.length / 2)
      var i   = 0
      while i < out.length do
        out(i) = Integer.parseInt(s.substring(i * 2, i * 2 + 2), 16).toByte
        i += 1
      out
end WordTransform
