package heddle.internal.h2

import zio.Chunk

/** RFC 7541. Encoder uses literals without indexing. Decoder handles static table, literals, Huffman. */
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

  def decode(block: Chunk[Byte]): Chunk[(String, String)] =
    val raw = block.toArray
    val out = Chunk.newBuilder[(String, String)]
    var i   = 0
    while i < raw.length do
      val b = raw(i) & 0xff
      if (b & 0x80) != 0 then
        val (idx, ni) = decodeInt(raw, i, 7)
        i = ni
        static(idx).foreach(out += _)
      else if (b & 0xc0) == 0x40 then
        val (idx, ni) = decodeInt(raw, i, 6)
        i = ni
        val (name, n2) = if idx == 0 then decodeString(raw, i) else (static(idx).map(_._1).getOrElse(""), i)
        if idx == 0 then i = n2
        val (value, n3) = decodeString(raw, i)
        i = n3
        out += (name -> value)
      else if (b & 0xe0) == 0x20 then
        val (_, ni) = decodeInt(raw, i, 5)
        i = ni
      else
        val (idx, ni) = decodeInt(raw, i, 4)
        i = ni
        val (name, n2) = if idx == 0 then decodeString(raw, i) else (static(idx).map(_._1).getOrElse(""), i)
        if idx == 0 then i = n2
        val (value, n3) = decodeString(raw, i)
        i = n3
        out += (name -> value)
      end if
    end while
    out.result()
  end decode

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

  private def decodeInt(raw: Array[Byte], from: Int, n: Int): (Int, Int) =
    if from >= raw.length then (0, from)
    else
      val max   = (1 << n) - 1
      val first = (raw(from) & 0xff) & max
      if first < max then (first, from + 1)
      else
        var i   = from + 1
        var m   = 0
        var acc = first
        while i < raw.length do
          val b = raw(i) & 0xff
          acc += (b & 0x7f) << m
          i += 1
          m += 7
          if (b & 0x80) == 0 then return (acc, i)
        (acc, i)
      end if

  private def decodeString(raw: Array[Byte], from: Int): (String, Int) =
    if from >= raw.length then ("", from)
    else
      val huff      = (raw(from) & 0x80) != 0
      val (len, ni) = decodeInt(raw, from, 7)
      val end       = (ni + len).min(raw.length)
      val slice     = raw.slice(ni, end)
      val s         =
        if huff then Huffman.decode(slice)
        else String(slice, java.nio.charset.StandardCharsets.ISO_8859_1)
      (s, end)

  private def staticIndex(name: String, value: String): Option[Int] =
    val n = name.toLowerCase
    Table.index.collectFirst { case ((nn, vv), i) if nn == n && vv == value => i }

  private def staticName(name: String): Option[Int] =
    val n = name.toLowerCase
    Table.index.collectFirst { case ((nn, _), i) if nn == n => i }

  private def static(idx: Int): Option[(String, String)] =
    Table.byIndex.get(idx)

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
