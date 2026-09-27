package heddle

import heddle.http.header.{BasicCredentials, HttpDate}
import heddle.route.PathCodec
import java.time.Instant
import java.util.UUID
import zio.*
import zio.test.*

object ParserLawsSpec extends ZIOSpecDefault:
  private val sunday = Instant.parse("1994-11-06T08:49:37Z")

  def spec = suite("total parsers")(
    suite("HTTP dates")(
      test("RFC 9110 §5.6.7: all three forms name the same instant"):
        assertTrue(
          HttpDate.parse("Sun, 06 Nov 1994 08:49:37 GMT").contains(sunday),
          HttpDate.parse("Sunday, 06-Nov-94 08:49:37 GMT").contains(sunday),
          HttpDate.parse("Sun Nov  6 08:49:37 1994").contains(sunday),
        )
      ,
      test("parse inverts render for any second from 1601 on"):
        check(
          Gen.long(
            Instant.parse("1601-01-01T00:00:00Z").getEpochSecond,
            Instant.parse("9999-12-31T23:59:59Z").getEpochSecond,
          )
        ) { s =>
          val t = Instant.ofEpochSecond(s)
          assertTrue(HttpDate.parse(HttpDate.render(t)).contains(t))
        }
      ,
      test("an impossible date is absent, never a throw"):
        assertTrue(
          HttpDate.parse("Mon, 30 Feb 2015 00:00:00 GMT").isEmpty,
          HttpDate.parse("Mon, 01 Jan 1600 00:00:00 GMT").isEmpty,
          HttpDate.parse("Mon, 01 Jan 2015 24:00:00 GMT").isEmpty,
          HttpDate.parse("not-a-date").isEmpty,
        )
      ,
      test("any text parses or is absent"):
        check(Gen.string)(s => assertTrue(HttpDate.parse(s).forall(_.getEpochSecond >= -11644473600L))),
    ),
    suite("UUID path segments")(
      test("every UUID parses back from its text, in either hex case"):
        check(Gen.uuid) { u =>
          assertTrue(
            PathCodec.parseUuid(u.toString).contains(u),
            PathCodec.parseUuid(u.toString.toUpperCase).contains(u),
          )
        }
      ,
      test("only the 8-4-4-4-12 shape is a UUID"):
        assertTrue(
          PathCodec.parseUuid("1-2-3-4-5").isEmpty,
          PathCodec.parseUuid("00000000-0000-0000-0000-00000000000g").isEmpty,
          PathCodec.parseUuid("00000000000000000000000000000000").isEmpty,
          PathCodec.parseUuid("").isEmpty,
        )
      ,
      test("any text parses or is absent"):
        check(Gen.string)(s => assertTrue(PathCodec.parseUuid(s).forall(u => UUID.fromString(s) == u))),
    ),
    suite("Basic credentials")(
      test("parse inverts render for any user without a colon, and any password"):
        check(Gen.string.map(_.filterNot(_ == ':')), Gen.string) { (user, pass) =>
          assertTrue(BasicCredentials.parse(BasicCredentials.render(user, pass)).contains(BasicCredentials(user, pass)))
        }
      ,
      test("any text parses or is absent"):
        check(Gen.string)(s => assertTrue(BasicCredentials.parse(s).forall(c => !c.username.contains(':')))),
    ),
  ) @@ TestAspect.timeout(60.seconds)
end ParserLawsSpec
