package heddle

/** A size in bytes. Not a buffer. `64.K` is 65536, `10.M` is 10 * 1024 * 1024. */
opaque type BytesLength = Long

object BytesLength:
  def apply(n: Long): BytesLength = n
  def apply(n: Int): BytesLength  = n.toLong

  val Zero: BytesLength = 0L

  extension (self: BytesLength)
    def toLong: Long = self
    def toInt: Int   =
      if self > Int.MaxValue || self < Int.MinValue then
        throw IllegalArgumentException(s"BytesLength $self does not fit in Int")
      else self.toInt
    def <(other: BytesLength): Boolean       = self < other
    def >(other: BytesLength): Boolean       = self > other
    def +(other: BytesLength): BytesLength   = self + other
    def -(other: BytesLength): BytesLength   = self - other
    def max(other: BytesLength): BytesLength = math.max(self, other)
  end extension

  extension (n: Int)
    def B: BytesLength = n.toLong
    def K: BytesLength = n.toLong * 1024L
    def M: BytesLength = n.toLong * 1024L * 1024L
    def G: BytesLength = n.toLong * 1024L * 1024L * 1024L

  extension (n: Long)
    def B: BytesLength = n
    def K: BytesLength = n * 1024L
    def M: BytesLength = n * 1024L * 1024L
    def G: BytesLength = n * 1024L * 1024L * 1024L
end BytesLength
