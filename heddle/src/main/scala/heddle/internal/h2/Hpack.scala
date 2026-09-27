package heddle.internal.h2

import heddle.error.HpackError
import zio.Chunk

/** RFC 7541. The encoder sends literals without indexing, so it keeps no table; the decoder keeps the peer's. */
private[heddle] object Hpack:
  def encode(headers: Chunk[(String, String)]): Chunk[Byte] =
    val b = Array.newBuilder[Byte]
    headers.foreach { (n, v) =>
      staticIndex(n, v) match
        case Some(i) => encodeInt(b, i, 7, 0x80)
        case None    =>
          staticName(n) match
            case Some(i) =>
              encodeInt(b, i, 4, 0x00) // literal without indexing, name indexed
              encodeString(b, v)
            case None =>
              b += 0x00.toByte
              encodeString(b, n.toLowerCase)
              encodeString(b, v)
    }
    Chunk.fromArray(b.result())
  end encode

  /** Decodes one header block against the connection's dynamic table and returns the table the next block sees. */
  def decode(
      block: Chunk[Byte],
      table: HpackTable,
      maxList: Long,
  ): Either[HpackError, (Chunk[(String, String)], HpackTable)] =
    val raw = block.toArray
    @scala.annotation.tailrec
    def loop(
        i: Int,
        t: HpackTable,
        out: Chunk[(String, String)],
        listSize: Long,
    ): Either[HpackError, (Chunk[(String, String)], HpackTable)] =
      if listSize > maxList then Left(HpackError.ListTooLarge(listSize, maxList))
      else if i >= raw.length then Right((out, t))
      else
        val b = raw(i) & 0xff
        if (b & 0x80) != 0 then
          decodeInt(raw, i, 7).flatMap((idx, next) => t.get(idx).toRight(HpackError.BadIndex(idx)).map(_ -> next)) match
            case Left(e)              => Left(e)
            case Right((field, next)) => loop(next, t, out :+ field, listSize + HpackTable.cost(field))
        else if (b & 0xe0) == 0x20 then
          if out.nonEmpty then Left(HpackError.LateSizeUpdate)
          else
            decodeInt(raw, i, 5).flatMap((to, next) => t.resize(to).map(_ -> next)) match
              case Left(e)             => Left(e)
              case Right((resized, n)) => loop(n, resized, out, listSize)
        else
          val indexing = (b & 0xc0) == 0x40
          literal(raw, i, if indexing then 6 else 4, t) match
            case Left(e)              => Left(e)
            case Right((field, next)) =>
              loop(next, if indexing then t.add(field) else t, out :+ field, listSize + HpackTable.cost(field))
        end if
    loop(0, table, Chunk.empty, 0L)
  end decode

  /** A literal field: an indexed or literal name, then a literal value. */
  private def literal(
      raw: Array[Byte],
      at: Int,
      prefix: Int,
      t: HpackTable,
  ): Either[HpackError, ((String, String), Int)] =
    decodeInt(raw, at, prefix).flatMap { (idx, afterIndex) =>
      val name =
        if idx == 0 then decodeString(raw, afterIndex)
        else t.get(idx).map(_._1).toRight(HpackError.BadIndex(idx)).map(_ -> afterIndex)
      name.flatMap((n, afterName) => decodeString(raw, afterName).map((v, next) => ((n, v), next)))
    }

  private[h2] def static(idx: Int): Option[(String, String)] =
    Table.byIndex.get(idx)

  private def encodeInt(b: scala.collection.mutable.ArrayBuilder[Byte], value: Int, n: Int, prefix: Int): Unit =
    val max = (1 << n) - 1
    if value < max then b += (prefix | value).toByte
    else
      b += (prefix | max).toByte
      var v = value - max
      while v >= 128 do
        b += ((v % 128) | 0x80).toByte
        v = v / 128
      b += v.toByte
  end encodeInt

  private def encodeString(b: scala.collection.mutable.ArrayBuilder[Byte], s: String): Unit =
    val bytes = s.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
    encodeInt(b, bytes.length, 7, 0x00)
    bytes.foreach(x => b += x)

  /** RFC 7541 §5.1 prefix integer; `(value, next index)`. */
  private def decodeInt(raw: Array[Byte], from: Int, n: Int): Either[HpackError, (Int, Int)] =
    if from >= raw.length then Left(HpackError.Truncated)
    else
      val max   = (1 << n) - 1
      val first = raw(from) & 0xff & max
      if first < max then Right((first, from + 1))
      else
        @scala.annotation.tailrec
        def more(i: Int, shift: Int, acc: Long): Either[HpackError, (Int, Int)] =
          if i >= raw.length then Left(HpackError.Truncated)
          else if shift > 28 then Left(HpackError.IntegerOverflow)
          else
            val b     = raw(i) & 0xff
            val value = acc + ((b & 0x7fL) << shift)
            if value > Int.MaxValue then Left(HpackError.IntegerOverflow)
            else if (b & 0x80) != 0 then more(i + 1, shift + 7, value)
            else Right((value.toInt, i + 1))
        more(from + 1, 0, first.toLong)
      end if

  private def decodeString(raw: Array[Byte], from: Int): Either[HpackError, (String, Int)] =
    if from >= raw.length then Left(HpackError.Truncated)
    else
      val huff = (raw(from) & 0x80) != 0
      decodeInt(raw, from, 7).flatMap { (len, start) =>
        if start.toLong + len > raw.length then Left(HpackError.Truncated)
        else
          val slice = raw.slice(start, start + len)
          val text  =
            if huff then Huffman.decode(slice).toRight(HpackError.BadHuffman)
            else Right(String(slice, java.nio.charset.StandardCharsets.ISO_8859_1))
          text.map(_ -> (start + len))
      }

  private def staticIndex(name: String, value: String): Option[Int] =
    val n = name.toLowerCase
    Table.index.collectFirst { case ((nn, vv), i) if nn == n && vv == value => i }

  private def staticName(name: String): Option[Int] =
    val n = name.toLowerCase
    Table.index.collectFirst { case ((nn, _), i) if nn == n => i }

  private object Table:
    val entries: List[(String, String)] = List(
      (":authority", ""),
      (":method", "GET"),
      (":method", "POST"),
      (":path", "/"),
      (":path", "/index.html"),
      (":scheme", "http"),
      (":scheme", "https"),
      (":status", "200"),
      (":status", "204"),
      (":status", "206"),
      (":status", "304"),
      (":status", "400"),
      (":status", "404"),
      (":status", "500"),
      ("accept-charset", ""),
      ("accept-encoding", "gzip, deflate"),
      ("accept-language", ""),
      ("accept-ranges", ""),
      ("accept", ""),
      ("access-control-allow-origin", ""),
      ("age", ""),
      ("allow", ""),
      ("authorization", ""),
      ("cache-control", ""),
      ("content-disposition", ""),
      ("content-encoding", ""),
      ("content-language", ""),
      ("content-length", ""),
      ("content-location", ""),
      ("content-range", ""),
      ("content-type", ""),
      ("cookie", ""),
      ("date", ""),
      ("etag", ""),
      ("expect", ""),
      ("expires", ""),
      ("from", ""),
      ("host", ""),
      ("if-match", ""),
      ("if-modified-since", ""),
      ("if-none-match", ""),
      ("if-range", ""),
      ("if-unmodified-since", ""),
      ("last-modified", ""),
      ("link", ""),
      ("location", ""),
      ("max-forwards", ""),
      ("proxy-authenticate", ""),
      ("proxy-authorization", ""),
      ("range", ""),
      ("referer", ""),
      ("refresh", ""),
      ("retry-after", ""),
      ("server", ""),
      ("set-cookie", ""),
      ("strict-transport-security", ""),
      ("transfer-encoding", ""),
      ("user-agent", ""),
      ("vary", ""),
      ("via", ""),
      ("www-authenticate", ""),
    )
    val byIndex: Map[Int, (String, String)]  = entries.zipWithIndex.map { case (p, i) => (i + 1) -> p }.toMap
    val index: List[((String, String), Int)] = entries.zipWithIndex.map { case (p, i) => p -> (i + 1) }
  end Table
end Hpack
