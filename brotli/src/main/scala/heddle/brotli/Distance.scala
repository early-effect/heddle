package heddle.brotli

/** RFC 7932 §4 distances with NPOSTFIX=0 and NDIRECT=0. Alphabet size 64. */
private[brotli] object Distance:
  val Alphabet: Int     = 16 + 48
  val AlphabetBits: Int = 6

  final case class Encoded(symbol: Int, extraBits: Int, extra: Int)

  def encode(d: Int): Encoded =
    var dcode = 16
    while dcode < Alphabet && !covers(dcode, d) do dcode += 1
    if dcode < Alphabet then
      val ndistbits = extraBits(dcode)
      Encoded(dcode, ndistbits, d - offset(dcode) - 1)
    else Encoded(16, 1, 0)

  /** The distance a long code and its extra bits name. Codes under 16 are short codes, relative to the ring. */
  def decode(symbol: Int, extra: Int): Option[Int] =
    Option.when(symbol >= 16)(offset(symbol) + extra + 1)

  private def covers(dcode: Int, d: Int): Boolean =
    d >= offset(dcode) + 1 && d <= offset(dcode) + (1 << extraBits(dcode))

  private def offset(symbol: Int): Int = ((2 + ((symbol - 16) & 1)) << extraBits(symbol)) - 4

  def extraBits(symbol: Int): Int = 1 + ((symbol - 16) >> 1)
end Distance
