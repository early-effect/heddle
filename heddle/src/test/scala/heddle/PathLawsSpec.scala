package heddle

import java.util.UUID
import zio.*
import zio.test.*

object PathLawsSpec extends ZIOSpecDefault:
  private val segment = Gen.oneOf(
    Gen.alphaNumericStringBounded(1, 6),
    Gen.int.map(_.toString),
    Gen.uuid.map(_.toString),
    Gen.elements("", "users", "posts", "-1", "x y"),
  )
  private val anyPath = Gen.listOfBounded(0, 6)(segment).map(ss => Path(Chunk.fromIterable(ss)))
  private val word    = Gen.alphaNumericStringBounded(1, 8)

  private val one   = "users" / int("id")
  private val two   = "users" / int("id") / "posts" / string("slug")
  private val three = "u" / long("n") / uuid("key") / "v" / int("rev")

  def spec = suite("Path laws")(
    suite("Combiner")(
      test("separate inverts combine, for Unit on either side, pairs, and every tuple arity"):
        check(Gen.int, word, Gen.long, Gen.boolean) { (i, s, l, b) =>
          val left  = summon[Combiner[Unit, Int]]
          val right = summon[Combiner[Int, Unit]]
          val pair  = summon[Combiner[Int, String]]
          val three = summon[Combiner[(Int, String), Long]]
          val four  = summon[Combiner[(Int, String, Long), Boolean]]
          assertTrue(
            left.separate(left.combine((), i)) == ((), i),
            right.separate(right.combine(i, ())) == (i, ()),
            pair.separate(pair.combine(i, s)) == (i, s),
            three.combine((i, s), l) == (i, s, l),
            three.separate((i, s, l)) == ((i, s), l),
            four.separate(four.combine((i, s, l), b)) == ((i, s, l), b),
          )
        }
      ,
      test("inputs are flat: a path of three captures is a triple, not nested pairs"):
        typeCheck(
          """val p: PathCodec[(Long, java.util.UUID, Int)] = "u" / long("n") / uuid("key") / "v" / int("rev")"""
        )
          .map(r => assertTrue(r.isRight))
      ,
      test("grouping codecs yourself keeps the group as one value"):
        val grouped = int("a") / (int("b") / int("c"))
        assertTrue(
          grouped.matches(Path.decode("/1/2/3")).contains((1, (2, 3))),
          grouped.encode((1, (2, 3))).render == "/1/2/3",
        ),
    ),
    suite("PathCodec")(
      test("matches inverts encode for one, two, and three captures"):
        check(Gen.int, word, Gen.long, Gen.uuid) { (i, s, l, u) =>
          assertTrue(
            one.matches(one.encode(i)).contains(i),
            two.matches(two.encode((i, s))).contains((i, s)),
            three.matches(three.encode((l, u, i))).contains((l, u, i)),
          )
        }
      ,
      test("the unrolled matcher agrees with the composed reader on any path"):
        val fast = (
          PathCodec.specialize("users" / int("id")),
          PathCodec.specialize("users" / int("id") / "posts" / string("slug")),
        )
        check(anyPath) { p =>
          assertTrue(
            fast._1.specialized,
            fast._2.specialized,
            fast._1.matches(p) == one.matches(p),
            fast._2.matches(p) == two.matches(p),
          )
        }
      ,
      test("a path matches only its own length, and trailing takes the rest"):
        val rest = "files" / trailing
        check(anyPath) { p =>
          assertTrue(
            one.matches(p).isDefined == (p.segments.length == 2 && p
              .segments(0) == "users" && p.segments(1).toIntOption.isDefined),
            rest.matches(Path("files" +: p.segments)).contains(p),
          )
        }
      ,
      test("a literal-only path has its value up front"):
        val ping = PathCodec.lit("health") / "live"
        assertTrue(ping.matches(Path.decode("/health/live")).contains(()), ping.matches(Path.decode("/health")).isEmpty),
    ),
  ) @@ TestAspect.timeout(60.seconds)
end PathLawsSpec
