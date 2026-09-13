package heddle.brotli

/** Huffman decode tables, RFC 7932 §3. */
private[brotli] object HuffmanTable:
  val MaxTableSize: Int = 1080
  val TableBits: Int    = 8
  val TableMask: Int    = 0xff
  val MaxLength: Int    = 15

  final class Table(val data: Array[Int], val offset: Int):
    def read(br: BitReader): Int =
      val v    = br.peekBits(16)
      var off  = offset + (v & TableMask)
      val bits = data(off) >>> 16
      val sym  = data(off) & 0xffff
      if bits <= TableBits then
        br.dropBits(bits)
        sym
      else
        off += sym
        val mask = (1 << bits) - 1
        off += (v & mask) >>> TableBits
        br.dropBits((data(off) >>> 16) + TableBits)
        data(off) & 0xffff
    end read
  end Table

  def build(codeLengths: Array[Int], rootBits: Int): Table =
    val table = Array.ofDim[Int](MaxTableSize)
    buildInto(table, 0, rootBits, codeLengths)
    Table(table, 0)

  def readCode(br: BitReader, alphabetSize: Int): Table =
    val lengths = Array.ofDim[Int](alphabetSize)
    val kind    = br.readBits(2)
    if kind == 1 then readSimple(br, lengths, alphabetSize)
    else readComplex(br, kind, lengths, alphabetSize)
    build(lengths, TableBits)

  private def readSimple(br: BitReader, lengths: Array[Int], alphabetSize: Int): Unit =
    var maxBitsCounter = alphabetSize - 1
    var maxBits        = 0
    while maxBitsCounter != 0 do
      maxBitsCounter >>= 1
      maxBits += 1
    val nsym    = br.readBits(2) + 1
    val symbols = Array.ofDim[Int](4)
    var i       = 0
    while i < nsym do
      symbols(i) = br.readBits(maxBits) % alphabetSize
      lengths(symbols(i)) = 2
      i += 1
    lengths(symbols(0)) = 1
    nsym match
      case 1 => ()
      case 2 =>
        if symbols(0) == symbols(1) then throw BrotliException("duplicate simple symbol")
        lengths(symbols(1)) = 1
      case 3 =>
        if symbols(0) == symbols(1) || symbols(0) == symbols(2) || symbols(1) == symbols(2) then
          throw BrotliException("duplicate simple symbol")
      case _ =>
        if Set(symbols(0), symbols(1), symbols(2), symbols(3)).size != 4 then
          throw BrotliException("duplicate simple symbol")
        if br.readBits(1) == 1 then
          lengths(symbols(2)) = 3
          lengths(symbols(3)) = 3
        else lengths(symbols(0)) = 2
    end match
  end readSimple

  private val CodeLengthOrder =
    Array(1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  /** Static Huffman for the 2–4 bit code-length-code lengths (RFC 7932). */
  private val FixedTable = Array(
    0x020000, 0x020004, 0x020003, 0x030002, 0x020000, 0x020004, 0x020003, 0x040001, 0x020000, 0x020004, 0x020003,
    0x030002, 0x020000, 0x020004, 0x020003, 0x040005,
  )

  private def readComplex(br: BitReader, skip: Int, lengths: Array[Int], alphabetSize: Int): Unit =
    val clcl  = Array.ofDim[Int](18)
    var space = 32
    var ncode = 0
    var i     = skip
    while i < 18 && space > 0 do
      val p = br.peekBits(4)
      br.dropBits(FixedTable(p) >>> 16)
      val v = FixedTable(p) & 0xffff
      clcl(CodeLengthOrder(i)) = v
      if v != 0 then
        space -= 32 >> v
        ncode += 1
      i += 1
    if !(ncode == 1 || space == 0) then throw BrotliException("Can't readHuffmanCode")
    readCodeLengths(br, clcl, lengths, alphabetSize)
  end readComplex

  private def readCodeLengths(br: BitReader, clcl: Array[Int], lengths: Array[Int], numSymbols: Int): Unit =
    val table = Array.ofDim[Int](32)
    buildInto(table, 0, 5, clcl)
    var symbol        = 0
    var prev          = 8
    var repeat        = 0
    var repeatCodeLen = 0
    var space         = 32768
    while symbol < numSymbols && space > 0 do
      val p = br.peekBits(5)
      br.dropBits(table(p) >>> 16)
      val codeLen = table(p) & 0xffff
      if codeLen < 16 then
        repeat = 0
        lengths(symbol) = codeLen
        symbol += 1
        if codeLen != 0 then
          prev = codeLen
          space -= 32768 >> codeLen
      else
        val extraBits = codeLen - 14
        val newLen    = if codeLen == 16 then prev else 0
        if repeatCodeLen != newLen then
          repeat = 0
          repeatCodeLen = newLen
        val old = repeat
        if repeat > 0 then
          repeat -= 2
          repeat <<= extraBits
        repeat += br.readBits(extraBits) + 3
        val delta = repeat - old
        if symbol + delta > numSymbols then throw BrotliException("symbol + repeatDelta > numSymbols")
        var k = 0
        while k < delta do
          lengths(symbol) = repeatCodeLen
          symbol += 1
          k += 1
        if repeatCodeLen != 0 then space -= delta << (15 - repeatCodeLen)
      end if
    end while
    if space != 0 then throw BrotliException("Unused space")
  end readCodeLengths

  private def getNextKey(key: Int, len: Int): Int =
    var step = 1 << (len - 1)
    while (key & step) != 0 do step >>= 1
    (key & (step - 1)) + step

  private def replicate(table: Array[Int], offset: Int, step: Int, end0: Int, item: Int): Unit =
    var end = end0
    while
      end -= step
      table(offset + end) = item
      end > 0
    do ()

  private def nextTableBitSize(count: Array[Int], len0: Int, rootBits: Int): Int =
    var len  = len0
    var left = 1 << (len - rootBits)
    while len < MaxLength do
      left -= count(len)
      if left <= 0 then return len - rootBits
      len += 1
      left <<= 1
    len - rootBits

  private def buildInto(root: Array[Int], tableOffset: Int, rootBits: Int, codeLengths: Array[Int]): Unit =
    val n      = codeLengths.length
    val sorted = Array.ofDim[Int](n)
    val count  = Array.ofDim[Int](MaxLength + 1)
    val offset = Array.ofDim[Int](MaxLength + 1)
    var symbol = 0
    while symbol < n do
      count(codeLengths(symbol)) += 1
      symbol += 1
    offset(1) = 0
    var len = 1
    while len < MaxLength do
      offset(len + 1) = offset(len) + count(len)
      len += 1
    symbol = 0
    while symbol < n do
      if codeLengths(symbol) != 0 then
        val l = codeLengths(symbol)
        sorted(offset(l)) = symbol
        offset(l) += 1
      symbol += 1
    var tableBits = rootBits
    var tableSize = 1 << tableBits
    var totalSize = tableSize
    if offset(MaxLength) == 1 then
      var key = 0
      while key < totalSize do
        root(tableOffset + key) = sorted(0)
        key += 1
      return
    var key = 0
    symbol = 0
    len = 1
    var step = 2
    while len <= rootBits do
      while count(len) > 0 do
        replicate(root, tableOffset + key, step, tableSize, (len << 16) | sorted(symbol))
        symbol += 1
        key = getNextKey(key, len)
        count(len) -= 1
      len += 1
      step <<= 1
    val mask          = totalSize - 1
    var low           = -1
    var currentOffset = tableOffset
    len = rootBits + 1
    step = 2
    while len <= MaxLength do
      while count(len) > 0 do
        if (key & mask) != low then
          currentOffset += tableSize
          tableBits = nextTableBitSize(count, len, rootBits)
          tableSize = 1 << tableBits
          totalSize += tableSize
          low = key & mask
          root(tableOffset + low) = ((tableBits + rootBits) << 16) | (currentOffset - tableOffset - low)
        replicate(root, currentOffset + (key >> rootBits), step, tableSize, ((len - rootBits) << 16) | sorted(symbol))
        symbol += 1
        key = getNextKey(key, len)
        count(len) -= 1
      end while
      len += 1
      step <<= 1
    end while
  end buildInto
end HuffmanTable
