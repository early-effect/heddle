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

  /** Total: corrupt input is its `BrotliError`, and output that would pass `limit` bytes is `OverLimit`. */
  def decode(data: Array[Byte], limit: Long): Either[BrotliError, Array[Byte]] =
    for
      (dict, lookup) <- Dict.loaded
      transforms     <- WordTransform.all
      out            <- run(BitReader(data), limit, Tables(dict, lookup, transforms))
    yield out

  private final class Tables(val dict: Array[Byte], val lookup: Array[Int], val transforms: Array[WordTransform])

  private def run(br: BitReader, limit: Long, tables: Tables): Either[BrotliError, Array[Byte]] =
    val wbits   = windowBits(br)
    val maxBack = (1 << wbits) - 16
    val out     = ArrayBuffer.empty[Byte]
    val distRb  = Array(16, 15, 11, 4)
    var distI   = 0
    var last    = false
    while !last && br.ok do
      val hdr = metablock(br)
      last = hdr.last
      if !hdr.metadata && out.length.toLong + hdr.length > limit then br.fail(BrotliError.OverLimit)
      else if hdr.length == 0 || !br.ok then ()
      else if hdr.metadata then
        br.jumpToByteBoundary()
        val _ = br.copyBytes(hdr.length)
      else if hdr.uncompressed then
        br.jumpToByteBoundary()
        out ++= br.copyBytes(hdr.length)
      else distI = compressed(br, out, hdr.length, maxBack, distRb, distI, tables)
    end while
    br.jumpToByteBoundary()
    br.failure.toLeft(out.toArray)
  end run

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
    if last && br.readBits(1) != 0 then Header(true, 0, false, false)
    else
      val nibbles = br.readBits(2) + 4
      if nibbles == 7 then
        if br.readBits(1) != 0 then br.fail(BrotliError.ReservedBit)
        val sizeBytes = br.readBits(2)
        var len       = 0
        var i         = 0
        while i < sizeBytes do
          val bits = br.readBits(8)
          if bits == 0 && i + 1 == sizeBytes && sizeBytes > 1 then br.fail(BrotliError.ExuberantNibble)
          len |= bits << (i * 8)
          i += 1
        Header(last, if sizeBytes == 0 then 0 else len + 1, uncompressed = false, metadata = true)
      else
        var len = 0
        var i   = 0
        while i < nibbles do
          val bits = br.readBits(4)
          if bits == 0 && i + 1 == nibbles && nibbles > 4 then br.fail(BrotliError.ExuberantNibble)
          len |= bits << (i * 4)
          i += 1
        val uncomp = if last then false else br.readBits(1) == 1
        Header(last, len + 1, uncomp, metadata = false)
      end if
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
      tables: Tables,
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
    val lookup                = tables.lookup
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

    while mlen > 0 && br.ok do
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
        while ins < insertLen && br.ok do
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
        while ins < insertLen && br.ok do
          if blen(0) == 0 then
            val bt = switchType(0)
            ctxSlice = bt << LiteralCtxBits
            ctxOff1 = Dict.LookupOffsets(ctxModes(bt))
            ctxOff2 = Dict.LookupOffsets(ctxModes(bt) + 1)
          val p1  = if out.isEmpty then 0 else out(out.length - 1) & 0xff
          val p2  = if out.length < 2 then 0 else out(out.length - 2) & 0xff
          val idx = litMap(ctxSlice + (lookup(ctxOff1 + p1) | lookup(ctxOff2 + p2))) & 0xff
          blen(0) -= 1
          out += litTrees(idx).read(br).toByte
          ins += 1
      end if
      mlen -= insertLen
      if mlen > 0 && br.ok then
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
        val distance    = translateShort(distCode, distRb, distI)
        val maxDistance = math.min(out.length, maxBack)
        if distance <= 0 then br.fail(BrotliError.BadDistance)
        else
          if distCode > 0 then
            distRb(distI & 3) = distance
            distI += 1
          if distance > maxDistance then mlen -= dictionaryWord(br, out, distance - maxDistance - 1, copyLen, tables)
          else if copyLen > mlen then br.fail(BrotliError.BadReference)
          else
            var k = 0
            while k < copyLen do
              out += out(out.length - distance)
              k += 1
            mlen -= copyLen
          end if
        end if
      end if
    end while
    distI
  end compressed

  /** Appends a transformed static-dictionary word and returns its length, or fails the reader and returns 0. */
  private def dictionaryWord(br: BitReader, out: ArrayBuffer[Byte], wordId: Int, copyLen: Int, tables: Tables): Int =
    if copyLen < Dict.MinWordLength || copyLen > Dict.MaxWordLength then
      br.fail(BrotliError.BadReference)
      0
    else
      val shift   = Dict.SizeBitsByLength(copyLen)
      val wordIdx = wordId & ((1 << shift) - 1)
      val txIdx   = wordId >>> shift
      if txIdx >= tables.transforms.length then
        br.fail(BrotliError.BadReference)
        0
      else
        val tmp  = Array.ofDim[Byte](Dict.MaxTransformed)
        val from = Dict.OffsetsByLength(copyLen) + wordIdx * copyLen
        val nout = WordTransform(tmp, 0, tables.dict, from, copyLen, tables.transforms(txIdx))
        var k    = 0
        while k < nout do
          out += tmp(k)
          k += 1
        nout
      end if
    end if
  end dictionaryWord

  private def readBlockLength(table: HuffmanTable.Table, br: BitReader): Int =
    val code = table.read(br)
    BlockLenBase(code) + br.readBits(BlockLenExtra(code))

  private def translateShort(code: Int, ring: Array[Int], index: Int): Int =
    if code < DistanceShort then
      val idx = (index + DistShortIndex(code)) & 3
      ring(idx) + DistShortValue(code)
    else code - DistanceShort + 1

  /** A context map whose every entry names one of its `ntrees` trees. */
  private def contextMap(br: BitReader, size: Int): (Array[Byte], Int) =
    val ntrees = varLenByte(br) + 1
    val map    = Array.ofDim[Byte](size)
    if ntrees > 1 then
      val rle    = br.readBits(1) == 1
      val maxRun = if rle then br.readBits(4) + 1 else 0
      val table  = HuffmanTable.readCode(br, ntrees + maxRun)
      var i      = 0
      while i < size && br.ok do
        val code = table.read(br)
        if code == 0 then
          map(i) = 0
          i += 1
        else if code <= maxRun then
          val reps = (1 << code) + br.readBits(code)
          if i + reps > size then br.fail(BrotliError.BadContextMap)
          else i += reps
        else
          map(i) = (code - maxRun).toByte
          i += 1
        end if
      end while
      if br.readBits(1) == 1 then inverseMtf(map)
      if map.exists(v => (v & 0xff) >= ntrees) then br.fail(BrotliError.BadContextMap)
    end if
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
