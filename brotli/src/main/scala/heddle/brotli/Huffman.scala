package heddle.brotli

/** Canonical prefix codes, RFC 7932 §3. */
private[brotli] object Huffman:
  def lengths(freq: Array[Int], maxBits: Int): Array[Int] =
    val n   = freq.length
    val len = Array.fill(n)(0)
    val idx = freq.indices.filter(i => freq(i) > 0)
    if idx.isEmpty then len
    else if idx.length == 1 then
      len(idx.head) = 1
      len
    else
      val nodes = scala.collection.mutable.PriorityQueue.empty[(Int, Int)](using
        Ordering.by[(Int, Int), Int](_._1).reverse
      )
      idx.foreach(i => nodes.enqueue(freq(i) -> i))
      val parent = Array.fill(n + idx.length)(-1)
      var next   = n
      while nodes.size > 1 do
        val (fa, a) = nodes.dequeue()
        val (fb, b) = nodes.dequeue()
        parent(a) = next
        parent(b) = next
        nodes.enqueue((fa + fb) -> next)
        next += 1
      def depth(i: Int): Int =
        var d = 0
        var x = i
        while x < parent.length && parent(x) >= 0 do
          d += 1
          x = parent(x)
        d
      idx.foreach(i => len(i) = depth(i).max(1))
      limit(len, maxBits)
      len
    end if
  end lengths

  def prefixFree(len: Array[Int], code: Array[Int]): Boolean =
    val used = len.indices.filter(i => len(i) > 0)
    used.forall { i =>
      used.forall { j =>
        if i == j || len(i) > len(j) then true
        else (code(j) >>> (len(j) - len(i))) != code(i)
      }
    }

  def codes(len: Array[Int]): Array[Int] =
    val max = len.max
    val bl  = Array.fill(max + 1)(0)
    len.foreach(l => if l > 0 then bl(l) += 1)
    val next = Array.fill(max + 1)(0)
    var code = 0
    var bits = 1
    while bits <= max do
      code = (code + bl(bits - 1)) << 1
      next(bits) = code
      bits += 1
    val out = Array.fill(len.length)(0)
    var i   = 0
    while i < len.length do
      val l = len(i)
      if l > 0 then
        out(i) = next(l)
        next(l) += 1
      i += 1
    out
  end codes

  def writePrefixCode(w: BitWriter, len: Array[Int], alphabetBits: Int): Unit =
    var used = len.indices.filter(i => len(i) > 0)
    if used.length <= 1 then
      val a = used.headOption.getOrElse(0)
      val b = if a == 0 then 1 else 0
      len(a) = 1
      len(b) = 1
      used = IndexedSeq(a, b).sorted
    if used.length <= 4 then writeSimple(w, len, used, alphabetBits)
    else writeComplex(w, len)
  end writePrefixCode

  private def writeSimple(w: BitWriter, len: Array[Int], used: IndexedSeq[Int], alphabetBits: Int): Unit =
    val nsym = used.length.max(1)
    w.writeBits(2, 1)
    w.writeBits(2, (nsym - 1).toLong)
    val ordered =
      if nsym == 1 then used
      else used.sortBy(i => (len(i), i))
    ordered.foreach(i => w.writeBits(alphabetBits, i.toLong))
    if nsym == 4 then
      val lens = ordered.map(len)
      w.writeBits(1, if lens == IndexedSeq(2, 2, 2, 2) then 0 else 1)
  end writeSimple

  private def writeComplex(w: BitWriter, len: Array[Int]): Unit =
    var end = len.length
    while end > 0 && len(end - 1) == 0 do end -= 1
    val tokens = len.take(end.max(1))
    val hist   = Array.fill(18)(0)
    tokens.foreach(t => hist(t) += 1)
    val cl = lengths(hist, 5)
    var s  = 0
    while s < cl.length do
      if hist.lift(s).getOrElse(0) == 0 then cl(s) = 0
      s += 1
    if cl.count(_ > 0) < 2 then
      val some = hist.indices.filter(hist(_) > 0)
      some.headOption.foreach(i => cl(i) = 1)
      hist.indices.find(i => cl(i) == 0).foreach(i => cl(i) = 1)
    w.writeBits(2, 0)
    val order = Array(1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    var last  = 0
    var i     = 0
    while i < order.length do
      if cl(order(i)) != 0 then last = i
      i += 1
    var j = 0
    while j <= last do
      writeCodeLengthLen(w, cl(order(j)))
      j += 1
    val clCodes = codes(cl)
    tokens.foreach { t =>
      val l = cl(t)
      if l > 0 then w.writePrefix(l, clCodes(t))
    }
  end writeComplex

  private def writeCodeLengthLen(w: BitWriter, cl: Int): Unit =
    cl match
      case 0 => w.writeBits(2, 0)
      case 1 => w.writeBits(4, 7)
      case 2 => w.writeBits(3, 3)
      case 3 => w.writeBits(2, 2)
      case 4 => w.writeBits(2, 1)
      case 5 => w.writeBits(4, 15)
      case _ => w.writeBits(2, 0)

  private def limit(len: Array[Int], maxBits: Int): Unit =
    var overflow = true
    while overflow do
      overflow = false
      var i = 0
      while i < len.length do
        if len(i) > maxBits then
          overflow = true
          len(i) = maxBits
        i += 1
  end limit
end Huffman
