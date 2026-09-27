package heddle

import heddle.error.HpackError
import heddle.internal.h2.{Hpack, HpackTable}
import zio.*
import zio.test.*

object HpackSpec extends ZIOSpecDefault:
  private def hex(s: String): Chunk[Byte] =
    Chunk.fromIterable(s.filterNot(_.isWhitespace).grouped(2).map(Integer.parseInt(_, 16).toByte).toSeq)

  private val big = 1L << 20

  /** Decodes blocks in order on one table, as one connection would. */
  private def run(table: HpackTable, blocks: String*): Either[HpackError, (List[Chunk[(String, String)]], HpackTable)] =
    blocks.foldLeft[Either[HpackError, (List[Chunk[(String, String)]], HpackTable)]](Right((Nil, table))) {
      case (acc, block) =>
        acc.flatMap((seen, t) => Hpack.decode(hex(block), t, big).map((fields, next) => (seen :+ fields, next)))
    }

  private val first  = Chunk(":method" -> "GET", ":scheme" -> "http", ":path" -> "/", ":authority" -> "www.example.com")
  private val second = first :+ ("cache-control" -> "no-cache")
  private val third  =
    Chunk(
      ":method"    -> "GET",
      ":scheme"    -> "https",
      ":path"      -> "/index.html",
      ":authority" -> "www.example.com",
      "custom-key" -> "custom-value",
    )

  def spec = suite("HPACK")(
    test("RFC 7541 C.3: three requests share one dynamic table"):
      val out = run(
        HpackTable.empty,
        "828684410f7777772e6578616d706c652e636f6d",
        "828684be58086e6f2d6361636865",
        "828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565",
      )
      assertTrue(
        out.map(_._1) == Right(List(first, second, third)),
        out.map(_._2.size) == Right(164),
        out.map(_._2.entries.head) == Right("custom-key" -> "custom-value"),
      )
    ,
    test("RFC 7541 C.4: the same requests with Huffman strings"):
      val out = run(
        HpackTable.empty,
        "828684418cf1e3c2e5f23a6ba0ab90f4ff",
        "828684be5886a8eb10649cbf",
        "828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf",
      )
      assertTrue(out.map(_._1) == Right(List(first, second, third)), out.map(_._2.size) == Right(164))
    ,
    test("RFC 7541 C.5: a 256-octet table evicts its oldest entry"):
      val out = run(
        HpackTable(Vector.empty, 0, 256, 256),
        "4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d",
        "4803333037c1c0bf",
      )
      assertTrue(
        out.map(_._1.last) == Right(
          Chunk(
            ":status"       -> "307",
            "cache-control" -> "private",
            "date"          -> "Mon, 21 Oct 2013 20:13:21 GMT",
            "location"      -> "https://www.example.com",
          )
        ),
        out.map(_._2.size) == Right(222),
        out.map(_._2.entries.map(_._1)) == Right(Vector(":status", "location", "date", "cache-control")),
      )
    ,
    test("decode inverts encode for any fields, and our encoder never touches the table"):
      val name  = Gen.stringBounded(1, 12)(Gen.alphaNumericChar).map(_.toLowerCase)
      val value = Gen.stringBounded(0, 24)(Gen.char(' ', '\u00ff'))
      check(Gen.chunkOfBounded(0, 8)(name <*> value)) { fields =>
        assertTrue(Hpack.decode(Hpack.encode(fields), HpackTable.empty, big) == Right((fields, HpackTable.empty)))
      }
    ,
    test("malformed blocks are typed errors, never a throw or a dropped field"):
      assertTrue(
        run(HpackTable.empty, "80").left.toOption.contains(HpackError.BadIndex(0)),
        run(HpackTable.empty, "be").left.toOption.contains(HpackError.BadIndex(62)),
        run(HpackTable.empty, "410f7777").left.toOption.contains(HpackError.Truncated),
        run(HpackTable.empty, "8220").left.toOption.contains(HpackError.LateSizeUpdate),
        run(HpackTable.empty, "418aff").left.toOption.contains(HpackError.Truncated),
        run(HpackTable.empty, "41ff").left.toOption.contains(HpackError.Truncated),
      )
    ,
    test("a table size update over the advertised limit, and a header list over its limit, are refused"):
      val oversize = Hpack.decode(hex("3fe23f"), HpackTable.empty, big)
      val list     = Hpack.decode(hex("828684410f7777772e6578616d706c652e636f6d"), HpackTable.empty, 100)
      assertTrue(
        oversize.left.toOption.contains(HpackError.TableTooLarge(8193, 4096)),
        list.left.toOption.exists { case HpackError.ListTooLarge(_, 100) => true; case _ => false },
      ),
  ) @@ TestAspect.timeout(60.seconds)
end HpackSpec
