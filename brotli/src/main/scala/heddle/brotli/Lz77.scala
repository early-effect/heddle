package heddle.brotli

/** Greedy 4-byte hash matches, quality ~4. Distances are 1-based. */
private[brotli] object Lz77:
  /** Matches encoder WBITS=16: max backward reference is window minus 16. */
  val WindowBits: Int  = 16
  val MaxDistance: Int = (1 << WindowBits) - 16

  final case class Cmd(insert: Int, copy: Int, dist: Int, lits: Array[Byte]):
    def produced: Int = insert + (if dist != 0 then copy else 0)

  def compress(data: Array[Byte]): Array[Cmd] =
    val n      = data.length
    val table  = Array.fill(1 << 15)(-1)
    val out    = Array.newBuilder[Cmd]
    val lits   = Array.newBuilder[Byte]
    var insert = 0
    var i      = 0
    while i < n do
      var bestLen  = 0
      var bestDist = 0
      if i + 4 <= n then
        val h = hash4(data, i)
        val p = table(h)
        table(h) = i
        if p >= 0 && i - p <= MaxDistance && i - p >= 1 then
          var len = 0
          while i + len < n && data(p + len) == data(i + len) do len += 1
          if len >= 4 then
            bestLen = len.min(n - i)
            bestDist = i - p
      end if
      if bestLen >= 4 then
        out += Cmd(insert, bestLen, bestDist, lits.result())
        lits.clear()
        insert = 0
        var k = 1
        while k < bestLen && i + k + 4 <= n do
          table(hash4(data, i + k)) = i + k
          k += 1
        i += bestLen
      else
        lits += data(i)
        insert += 1
        i += 1
      end if
    end while
    if insert > 0 || out.result().isEmpty then out += Cmd(insert, 2, 0, lits.result())
    out.result()
  end compress

  def covers(data: Array[Byte], cmds: Array[Cmd]): Boolean =
    cmds.map(_.produced).sum == data.length

  def reconstruct(cmds: Array[Cmd]): Array[Byte] =
    val buf = scala.collection.mutable.ArrayBuffer.empty[Byte]
    cmds.foreach { c =>
      var i = 0
      while i < c.lits.length do
        buf += c.lits(i)
        i += 1
      if c.dist != 0 then
        var n = 0
        while n < c.copy do
          buf += buf(buf.length - c.dist)
          n += 1
    }
    buf.toArray
  end reconstruct

  private def hash4(data: Array[Byte], i: Int): Int =
    val v =
      ((data(i) & 0xff) << 24) | ((data(i + 1) & 0xff) << 16) | ((data(i + 2) & 0xff) << 8) | (data(i + 3) & 0xff)
    ((v * 0x1e35a7bd) >>> 17) & 0x7fff
end Lz77
