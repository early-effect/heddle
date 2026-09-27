package heddle.brotli

/** RFC 7932 dictionary word transforms. */
private[brotli] final case class WordTransform(prefix: Array[Byte], op: Int, suffix: Array[Byte])

private[brotli] object WordTransform:
  val Identity: Int       = 0
  val UppercaseFirst: Int = 10
  val UppercaseAll: Int   = 11

  /** The 121 RFC 7932 Appendix B transforms, one `prefix|op|suffix` line each (hex bytes). */
  lazy val all: Either[BrotliError, Array[WordTransform]] =
    Dict.resourceBytes("transforms.txt").flatMap { bytes =>
      val lines  = String(bytes, java.nio.charset.StandardCharsets.US_ASCII).split("\n").filter(_.nonEmpty)
      val parsed = lines.flatMap { line =>
        line.split("\\|", -1) match
          case Array(prefix, op, suffix) =>
            for
              p <- unhex(prefix)
              o <- op.toIntOption
              s <- unhex(suffix)
            yield WordTransform(p, o, s)
          case _ => None
      }
      Either.cond(parsed.length == lines.length, parsed, BrotliError.MissingResource("transforms.txt"))
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
      end while
    end if
    i = 0
    while i < tx.suffix.length do
      dst(offset) = tx.suffix(i)
      offset += 1
      i += 1
    offset - dstOff
  end apply

  private def unhex(s: String): Option[Array[Byte]] =
    Option.when(s.length % 2 == 0 && s.forall(c => Character.digit(c, 16) >= 0)) {
      Array.tabulate(s.length / 2)(i =>
        (Character.digit(s.charAt(i * 2), 16) * 16 + Character.digit(s.charAt(i * 2 + 1), 16)).toByte
      )
    }
end WordTransform
