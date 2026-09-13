package heddle.brotli

import scala.collection.mutable.ArrayBuffer

/** RFC 7932 decoder. Inverse of the encoder; also reads Google-quality streams. */
private[brotli] object Decoder:
  private val LiteralCodes     = 256
  private val InsertCopyCodes  = 704
  private val BlockLengthCodes = 26
  private val LiteralCtxBits   = 6
  private val DistanceCtxBits  = 2
  private val DistanceShort    = 16

  private val DistShortIndex = Array(3, 2, 1, 0, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2)
  private val DistShortValue = Array(0, 0, 0, 0, -1, 1, -2, 2, -3, 3, -1, 1, -2, 2, -3, 3)

  private val BlockLenBase =
    Array(1, 5, 9, 13, 17, 25, 33, 41, 49, 65, 81, 97, 113, 145, 177, 209, 241, 305, 369, 497, 753, 1265, 2289, 4337,
      8433, 16625)
  private val BlockLenExtra =
    Array(2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 24)

  def decode(data: Array[Byte]): Array[Byte] =
    val br      = BitReader(data)
    val wbits   = windowBits(br)
    val maxBack = (1 << wbits) - 16
    val out     = ArrayBuffer.empty[Byte]
    val distRb  = Array(16, 15, 11, 4)
    var distI   = 0
    var last    = false
    while !last do
      val hdr = metablock(br)
      last = hdr.last
      if hdr.length == 0 then ()
      else if hdr.metadata then
        br.jumpToByteBoundary()
        val _ = br.copyBytes(hdr.length)
      else if hdr.uncompressed then
        br.jumpToByteBoundary()
        out ++= br.copyBytes(hdr.length)
      else distI = compressed(br, out, hdr.length, maxBack, distRb, distI)
    end while
    br.jumpToByteBoundary()
    out.toArray
  end decode

  private def windowBits(br: BitReader): Int =
    if br.readBits(1) == 0 then 16
    else
      val n = br.readBits(3)
      if n != 0 then 17 + n
      else
        val m = br.readBits(3)
        if m != 0 then 8 + m
        else 17

  private final case class Header(last: Boolean, length: Int, uncompressed: Boolean, metadata: Boolean)

  private def metablock(br: BitReader): Header =
    val last = br.readBits(1) == 1
    if last && br.readBits(1) != 0 then return Header(true, 0, false, false)
    val nibbles = br.readBits(2) + 4
    if nibbles == 7 then
      if br.readBits(1) != 0 then throw BrotliException("corrupted reserved bit")
      val sizeBytes = br.readBits(2)
      var len       = 0
      var i         = 0
      while i < sizeBytes do
        val bits = br.readBits(8)
        if bits == 0 && i + 1 == sizeBytes && sizeBytes > 1 then throw BrotliException("exuberant nibble")
        len |= bits << (i * 8)
        i += 1
      Header(last, if sizeBytes == 0 then 0 else len + 1, uncompressed = false, metadata = true)
    else
      var len = 0
      var i   = 0
      while i < nibbles do
        val bits = br.readBits(4)
        if bits == 0 && i + 1 == nibbles && nibbles > 4 then throw BrotliException("exuberant nibble")
        len |= bits << (i * 4)
        i += 1
      val uncomp = if last then false else br.readBits(1) == 1
      Header(last, len + 1, uncomp, metadata = false)
    end if
  end metablock

  private def varLenByte(br: BitReader): Int =
    if br.readBits(1) == 0 then 0
    else
      val n = br.readBits(3)
      if n == 0 then 1 else br.readBits(n) + (1 << n)

  private def compressed(
      br: BitReader,
      out: ArrayBuffer[Byte],
      mlen0: Int,
      maxBack: Int,
      distRb: Array[Int],
      distI0: Int,
  ): Int =
    var mlen       = mlen0
    val ntype      = Array.ofDim[Int](3)
    val blen       = Array.ofDim[Int](3)
    val btypeTrees = Array.ofDim[HuffmanTable.Table](3)
    val blenTrees  = Array.ofDim[HuffmanTable.Table](3)
    val typeRb     = Array(1, 0, 1, 0, 1, 0)
    var t          = 0
    while t < 3 do
      ntype(t) = varLenByte(br) + 1
      blen(t) = 1 << 28
      if ntype(t) > 1 then
        btypeTrees(t) = HuffmanTable.readCode(br, ntype(t) + 2)
        blenTrees(t) = HuffmanTable.readCode(br, BlockLengthCodes)
        blen(t) = readBlockLength(blenTrees(t), br)
      t += 1
    val postfixBits = br.readBits(2)
    val nDirect     = DistanceShort + (br.readBits(4) << postfixBits)
    val postfixMask = (1 << postfixBits) - 1
    val nDistCodes  = nDirect + (48 << postfixBits)
    val ctxModes    = Array.ofDim[Int](ntype(0))
    var i           = 0
    while i < ntype(0) do
      ctxModes(i) = br.readBits(2) << 1
      i += 1
    val (litMap, nLitTrees)   = contextMap(br, ntype(0) << LiteralCtxBits)
    val (distMap, nDistTrees) = contextMap(br, ntype(2) << DistanceCtxBits)
    val litTrees              = Array.fill(nLitTrees)(HuffmanTable.readCode(br, LiteralCodes))
    val cmdTrees              = Array.fill(ntype(1))(HuffmanTable.readCode(br, InsertCopyCodes))
    val distTrees             = Array.fill(nDistTrees)(HuffmanTable.readCode(br, nDistCodes))
    var ctxSlice              = 0
    var distSlice             = 0
    var litTreeIdx            = 0
    var cmdTree               = cmdTrees(0)
    var ctxOff1               = Dict.LookupOffsets(ctxModes(0))
    var ctxOff2               = Dict.LookupOffsets(ctxModes(0) + 1)
    var distI                 = distI0
    val trivialLit            =
      var ok = true
      var j  = 0
      while j < litMap.length && ok do
        if (litMap(j) & 0xff) != (j >> LiteralCtxBits) then ok = false
        j += 1
      ok

    def switchType(kind: Int): Int =
      val off   = kind * 2
      var btype = btypeTrees(kind).read(br)
      blen(kind) = readBlockLength(blenTrees(kind), br)
      if btype == 1 then btype = typeRb(off + 1) + 1
      else if btype == 0 then btype = typeRb(off)
      else btype -= 2
      if btype >= ntype(kind) then btype -= ntype(kind)
      typeRb(off) = typeRb(off + 1)
      typeRb(off + 1) = btype
      btype
    end switchType

    while mlen > 0 do
      if blen(1) == 0 then
        val bt = switchType(1)
        cmdTree = cmdTrees(bt)
      blen(1) -= 1
      val cmd      = cmdTree.read(br)
      var rangeIdx = cmd >>> 6
      var distCode = 0
      if rangeIdx >= 2 then
        rangeIdx -= 2
        distCode = -1
      val insertCode = Command.InsertRangeLut(rangeIdx) + ((cmd >>> 3) & 7)
      val copyCode   = Command.CopyRangeLut(rangeIdx) + (cmd & 7)
      val insertLen  = Command.InsBase(insertCode) + br.readBits(Command.InsExtra(insertCode))
      val copyLen    = Command.CopyBase(copyCode) + br.readBits(Command.CopyExtra(copyCode))
      var ins        = 0
      if trivialLit then
        while ins < insertLen do
          if blen(0) == 0 then
            val bt = switchType(0)
            ctxSlice = bt << LiteralCtxBits
            litTreeIdx = litMap(ctxSlice) & 0xff
            ctxOff1 = Dict.LookupOffsets(ctxModes(bt))
            ctxOff2 = Dict.LookupOffsets(ctxModes(bt) + 1)
          blen(0) -= 1
          out += litTrees(litTreeIdx).read(br).toByte
          ins += 1
      else
        while ins < insertLen do
          if blen(0) == 0 then
            val bt = switchType(0)
            ctxSlice = bt << LiteralCtxBits
            ctxOff1 = Dict.LookupOffsets(ctxModes(bt))
            ctxOff2 = Dict.LookupOffsets(ctxModes(bt) + 1)
          val p1  = if out.isEmpty then 0 else out(out.length - 1) & 0xff
          val p2  = if out.length < 2 then 0 else out(out.length - 2) & 0xff
          val idx = litMap(ctxSlice + (Dict.contextLookup(ctxOff1 + p1) | Dict.contextLookup(ctxOff2 + p2))) & 0xff
          blen(0) -= 1
          out += litTrees(idx).read(br).toByte
          ins += 1
      end if
      mlen -= insertLen
      if mlen <= 0 then return distI
      if distCode < 0 then
        if blen(2) == 0 then
          val bt = switchType(2)
          distSlice = bt << DistanceCtxBits
        blen(2) -= 1
        val dctx = if copyLen > 4 then 3 else copyLen - 2
        distCode = distTrees(distMap(distSlice + dctx) & 0xff).read(br)
        if distCode >= nDirect then
          distCode -= nDirect
          val postfix = distCode & postfixMask
          distCode >>>= postfixBits
          val n      = (distCode >>> 1) + 1
          val offset = ((2 + (distCode & 1)) << n) - 4
          distCode = nDirect + postfix + ((offset + br.readBits(n)) << postfixBits)
      end if
      val distance = translateShort(distCode, distRb, distI)
      if distance < 0 then throw BrotliException("negative distance")
      val maxDistance = math.min(out.length, maxBack)
      if distCode > 0 then
        distRb(distI & 3) = distance
        distI += 1
      if distance > maxDistance then
        if copyLen < Dict.MinWordLength || copyLen > Dict.MaxWordLength then
          throw BrotliException("invalid backward reference")
        val wordOff = Dict.OffsetsByLength(copyLen)
        val shift   = Dict.SizeBitsByLength(copyLen)
        val wordId  = distance - maxDistance - 1
        val wordIdx = wordId & ((1 << shift) - 1)
        val txIdx   = wordId >>> shift
        if txIdx >= WordTransform.all.length then throw BrotliException("invalid backward reference")
        val tmp  = Array.ofDim[Byte](Dict.MaxTransformed)
        val nout = WordTransform(
          tmp,
          0,
          Dict.data,
          wordOff + wordIdx * copyLen,
          copyLen,
          WordTransform.all(txIdx),
        )
        var k = 0
        while k < nout do
          out += tmp(k)
          k += 1
        mlen -= nout
      else
        if copyLen > mlen then throw BrotliException("invalid backward reference")
        var k = 0
        while k < copyLen do
          out += out(out.length - distance)
          k += 1
        mlen -= copyLen
      end if
    end while
    distI
  end compressed

  private def readBlockLength(table: HuffmanTable.Table, br: BitReader): Int =
    val code = table.read(br)
    BlockLenBase(code) + br.readBits(BlockLenExtra(code))

  private def translateShort(code: Int, ring: Array[Int], index: Int): Int =
    if code < DistanceShort then
      val idx = (index + DistShortIndex(code)) & 3
      ring(idx) + DistShortValue(code)
    else code - DistanceShort + 1

  private def contextMap(br: BitReader, size: Int): (Array[Byte], Int) =
    val ntrees = varLenByte(br) + 1
    val map    = Array.ofDim[Byte](size)
    if ntrees == 1 then return (map, 1)
    val rle    = br.readBits(1) == 1
    val maxRun = if rle then br.readBits(4) + 1 else 0
    val table  = HuffmanTable.readCode(br, ntrees + maxRun)
    var i      = 0
    while i < size do
      val code = table.read(br)
      if code == 0 then
        map(i) = 0
        i += 1
      else if code <= maxRun then
        var reps = (1 << code) + br.readBits(code)
        while reps != 0 do
          if i >= size then throw BrotliException("corrupted context map")
          map(i) = 0
          i += 1
          reps -= 1
      else
        map(i) = (code - maxRun).toByte
        i += 1
      end if
    end while
    if br.readBits(1) == 1 then inverseMtf(map)
    (map, ntrees)
  end contextMap

  private def inverseMtf(v: Array[Byte]): Unit =
    val mtf = Array.tabulate(256)(identity)
    var i   = 0
    while i < v.length do
      val index = v(i) & 0xff
      v(i) = mtf(index).toByte
      if index != 0 then
        val value = mtf(index)
        var j     = index
        while j > 0 do
          mtf(j) = mtf(j - 1)
          j -= 1
        mtf(0) = value
      i += 1
    end while
  end inverseMtf
end Decoder
