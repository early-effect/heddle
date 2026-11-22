package heddle.mcp.events

import zio.Chunk

/** Global unicast, as a function of the address bytes. Private, loopback, link-local, documentation, and multicast
  * ranges are rejected. A v4-mapped IPv6 address is judged by the embedded IPv4 address. `64:ff9b::/96` is rejected as
  * a whole.
  */
private[events] object AddressPolicy:
  def globallyRoutable(bytes: Chunk[Byte]): Boolean =
    bytes.length match
      case 4  => !specialV4(bytes)
      case 16 => !specialV6(bytes)
      case _  => false

  /** An IP literal, or `None` when `host` is a name that has to be resolved later. */
  def parseLiteral(host: String): Option[Chunk[Byte]] =
    val bare = if host.startsWith("[") && host.endsWith("]") then host.substring(1, host.length - 1) else host
    if bare.contains(':') then parseV6(bare)
    else if bare.forall(c => c.isDigit || c == '.') then parseV4(bare)
    else None

  private def specialV4(bytes: Chunk[Byte]): Boolean =
    octet(bytes, 0, 1, 2) match
      case None            => true
      case Some((a, b, c)) =>
        a == 0 || a == 10 || a == 127 ||
        (a == 100 && (b & 0xc0) == 64) ||
        (a == 169 && b == 254) ||
        (a == 172 && (b & 0xf0) == 16) ||
        (a == 192 && b == 0 && c == 0) ||
        (a == 192 && b == 0 && c == 2) ||
        (a == 192 && b == 168) ||
        (a == 198 && (b & 0xfe) == 18) ||
        (a == 198 && b == 51 && c == 100) ||
        (a == 203 && b == 0 && c == 113) ||
        (a & 0xf0) == 224 ||
        (a & 0xf0) == 240

  private def specialV6(bytes: Chunk[Byte]): Boolean =
    val allZero  = bytes.forall(_ == 0)
    val loopback = bytes.take(15).forall(_ == 0) && byteAt(bytes, 15).contains(1)
    val mapped   = bytes.take(10).forall(_ == 0) && byteAt(bytes, 10).contains(0xff) && byteAt(bytes, 11).contains(0xff)
    val nat64    = byteAt(bytes, 0).contains(0x00) && byteAt(bytes, 1).contains(0x64) &&
      byteAt(bytes, 2).contains(0xff) && byteAt(bytes, 3).contains(0x9b) && bytes.slice(4, 12).forall(_ == 0)
    val discard = byteAt(bytes, 0).contains(0x01) && bytes.slice(1, 8).forall(_ == 0)
    val doc     = byteAt(bytes, 0).contains(0x20) && byteAt(bytes, 1).contains(0x01) &&
      byteAt(bytes, 2).contains(0x0d) && byteAt(bytes, 3).contains(0xb8)
    val benchmark = byteAt(bytes, 0).contains(0x20) && byteAt(bytes, 1).contains(0x01) &&
      byteAt(bytes, 2).contains(0x00) && byteAt(bytes, 3).contains(0x02) && byteAt(bytes, 4).contains(0) &&
      byteAt(bytes, 5).contains(0)
    val ula       = byteAt(bytes, 0).exists(b => (b & 0xfe) == 0xfc)
    val linkLocal = byteAt(bytes, 0).contains(0xfe) && byteAt(bytes, 1).exists(b => (b & 0xc0) == 0x80)
    val multicast = byteAt(bytes, 0).contains(0xff)
    allZero || loopback || (mapped && !globallyRoutable(bytes.drop(12))) || nat64 || discard || doc || benchmark ||
    ula || linkLocal || multicast

  private def octet(bytes: Chunk[Byte], i: Int, j: Int, k: Int): Option[(Int, Int, Int)] =
    for
      a <- byteAt(bytes, i)
      b <- byteAt(bytes, j)
      c <- byteAt(bytes, k)
    yield (a, b, c)

  private def byteAt(bytes: Chunk[Byte], index: Int): Option[Int] =
    if index >= 0 && index < bytes.length then bytes.drop(index).headOption.map(_ & 0xff) else None

  private def parseV4(host: String): Option[Chunk[Byte]] =
    val parts = host.split("\\.", -1)
    if parts.length != 4 then None
    else
      val nums = parts.map(p => scala.util.Try(p.toInt).toOption.filter(n => p.nonEmpty && n >= 0 && n <= 255))
      if nums.exists(_.isEmpty) then None
      else Some(Chunk.fromArray(nums.flatMap(_.map(_.toByte))))

  private def parseV6(host: String): Option[Chunk[Byte]] =
    if host.count(_ == ':') < 2 || host.indexOf("::") != host.lastIndexOf("::") then None
    else
      val splitAt             = host.indexOf("::")
      val (leftRaw, rightRaw) =
        if splitAt < 0 then (host, "")
        else (host.take(splitAt), host.drop(splitAt + 2))
      (hextets(leftRaw), hextets(rightRaw)) match
        case (Some(left), Some(right)) =>
          val missing = 8 - (left.length + right.length)
          if splitAt < 0 && missing == 0 then Some(pack(left))
          else if splitAt >= 0 && missing > 0 then Some(pack(left ++ List.fill(missing)(0) ++ right))
          else None
        case _ => None

  private def hextets(raw: String): Option[List[Int]] =
    if raw.isEmpty then Some(Nil)
    else
      val parts = raw.split(":", -1).toList
      if parts.exists(_.isEmpty) then None
      else
        parts.lastOption match
          case Some(last) if last.contains('.') =>
            parseV4(last) match
              case Some(v4) if v4.length == 4 =>
                val pair = (i: Int, j: Int) =>
                  for
                    hi <- byteAt(v4, i)
                    lo <- byteAt(v4, j)
                  yield (hi << 8) | lo
                (pair(0, 1), pair(2, 3)) match
                  case (Some(hi), Some(lo)) => group(parts.dropRight(1)).map(_ :+ hi :+ lo)
                  case _                    => None
              case _ => None
          case _ => group(parts)

  private def group(parts: List[String]): Option[List[Int]] =
    parts.foldLeft[Option[List[Int]]](Some(Nil)) { (acc, part) =>
      acc.flatMap { soFar =>
        if part.length > 4 then None
        else scala.util.Try(Integer.parseInt(part, 16)).toOption.filter(_ >= 0).map(n => soFar :+ n)
      }
    }

  private def pack(hextets: List[Int]): Chunk[Byte] =
    Chunk.fromIterable(hextets.flatMap(n => List(((n >>> 8) & 0xff).toByte, (n & 0xff).toByte)))

end AddressPolicy
