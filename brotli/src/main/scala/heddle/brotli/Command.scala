package heddle.brotli

/** RFC 7932 §5 insert-and-copy length codes. */
private[brotli] object Command:
  final case class Insert(code: Int, extraBits: Int, extra: Int)
  final case class Copy(code: Int, extraBits: Int, extra: Int)
  final case class Encoded(id: Int, insert: Insert, copy: Copy, emitDistance: Boolean)

  val InsBase: Array[Int] =
    Array(0, 1, 2, 3, 4, 5, 6, 8, 10, 14, 18, 26, 34, 50, 66, 98, 130, 194, 322, 578, 1090, 2114, 6210, 22594)
  val InsExtra: Array[Int] = Array(0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 9, 10, 12, 14, 24)
  val CopyBase: Array[Int] =
    Array(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 18, 22, 30, 38, 54, 70, 102, 134, 198, 326, 582, 1094, 2118)
  val CopyExtra: Array[Int] = Array(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 9, 10, 24)

  /** Same LUT org.brotli.dec uses to split a 704-symbol id. */
  val InsertRangeLut: Array[Int] = Array(0, 0, 8, 8, 0, 16, 8, 16, 16)
  val CopyRangeLut: Array[Int]   = Array(0, 8, 0, 8, 16, 0, 16, 8, 16)

  def insert(n: Int): Insert =
    var c = 0
    while c < 23 && n > InsBase(c) + ((1 << InsExtra(c)) - 1) do c += 1
    Insert(c, InsExtra(c), n - InsBase(c))

  def copy(n: Int): Copy =
    val v = n.max(2)
    var c = 0
    while c < 23 && v > CopyBase(c) + ((1 << CopyExtra(c)) - 1) do c += 1
    Copy(c, CopyExtra(c), v - CopyBase(c))

  def encode(insertLen: Int, copyLen: Int, emitDistance: Boolean): Encoded =
    val ins = insert(insertLen)
    val cpy = copy(copyLen)
    Encoded(id(ins.code, cpy.code, emitDistance), ins, cpy, emitDistance)

  def id(ic: Int, cc: Int, emitDistance: Boolean): Int =
    if !emitDistance && ic < 8 && cc < 16 then if cc < 8 then (ic << 3) | cc else 64 + ((ic << 3) | (cc - 8))
    else
      val base =
        (ic / 8, cc / 8) match
          case (0, 0) => 128
          case (0, 1) => 192
          case (0, 2) => 384
          case (1, 0) => 256
          case (1, 1) => 320
          case (1, 2) => 512
          case (2, 0) => 448
          case (2, 1) => 576
          case _      => 640
      base + ((ic % 8) << 3) + (cc % 8)

  def decodeId(cmd: Int): (Int, Int, Boolean) =
    var rangeIdx = cmd >>> 6
    val hasDist  = rangeIdx >= 2
    if hasDist then rangeIdx -= 2
    val ic = InsertRangeLut(rangeIdx) + ((cmd >>> 3) & 7)
    val cc = CopyRangeLut(rangeIdx) + (cmd & 7)
    (ic, cc, hasDist)

  def insertLength(ins: Insert): Int = InsBase(ins.code) + ins.extra
  def copyLength(cpy: Copy): Int     = CopyBase(cpy.code) + cpy.extra
end Command
