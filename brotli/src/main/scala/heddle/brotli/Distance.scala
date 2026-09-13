package heddle.brotli

/** RFC 7932 §4 distances with NPOSTFIX=0 and NDIRECT=0. Alphabet size 64. */
private[brotli] object Distance:
  val Alphabet: Int     = 16 + 48
  val AlphabetBits: Int = 6

  final case class Encoded(symbol: Int, extraBits: Int, extra: Int)

  def encode(d: Int): Encoded =
    var dcode = 16
    while dcode < Alphabet do
      val ndistbits = extraBits(dcode)
      val hcode     = dcode - 16
      val offset    = ((2 + (hcode & 1)) << ndistbits) - 4
      val span      = 1 << ndistbits
      if d >= offset + 1 && d <= offset + span then return Encoded(dcode, ndistbits, d - offset - 1)
      dcode += 1
    Encoded(16, 1, 0)
  end encode

  def decode(symbol: Int, extra: Int): Int =
    if symbol < 16 then throw IllegalArgumentException(s"short code $symbol")
    val ndistbits = extraBits(symbol)
    val hcode     = symbol - 16
    val offset    = ((2 + (hcode & 1)) << ndistbits) - 4
    offset + extra + 1

  def extraBits(symbol: Int): Int = 1 + ((symbol - 16) >> 1)
end Distance
