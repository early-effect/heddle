package heddle

import zio.Chunk
import zio.test.*

object UrlLawsSpec extends ZIOSpecDefault:
  private val anyText: Gen[Any, String] =
    Gen.oneOf(Gen.string, Gen.string(Gen.elements('%', '+', '/', '?', '#', '=', '&', ' ', 'a', 'F', '0', '9', 'é')))

  private val label: Gen[Any, String] =
    Gen.stringBounded(1, 12)(Gen.alphaNumericChar).map(_.toLowerCase)

  private val host: Gen[Any, String] =
    Gen.oneOf(
      Gen.listOfBounded(1, 4)(label).map(_.mkString(".")),
      Gen.listOfN(4)(Gen.int(0, 255)).map(_.mkString(".")),
      Gen.elements("[::1]", "[2001:db8::7]", "[fe80::1:2]"),
    )

  private val segment: Gen[Any, String] = Gen.stringBounded(1, 10)(Gen.printableChar).filter(_ != ".")

  private val query: Gen[Any, QueryParams] =
    Gen.mapOfBounded(0, 4)(Gen.stringBounded(1, 6)(Gen.printableChar), Gen.listOfBounded(1, 2)(Gen.string)).map { m =>
      QueryParams(m.view.mapValues(Chunk.fromIterable).toMap)
    }

  private val absolute: Gen[Any, Url] =
    for
      scheme <- Gen.elements(Scheme.Http, Scheme.Https)
      h      <- host
      port   <- Gen.option(Gen.int(1, 65535)).map(_.filterNot(_ == scheme.defaultPort))
      path   <- Gen.listOfBounded(0, 5)(segment)
      q      <- query
    yield Url(Path(Chunk.fromIterable(path)), q, Some(scheme), Some(h), port)

  def spec = suite("URL laws")(
    test("percent decoding inverts encoding"):
      check(anyText)(s => assertTrue(UrlEncoding.decode(UrlEncoding.encode(s)) == s))
    ,
    test("form decoding inverts form encoding"):
      check(anyText)(s => assertTrue(UrlEncoding.decodeForm(UrlEncoding.encodeForm(s)) == s))
    ,
    test("decoding never throws and keeps a stray percent literal"):
      check(anyText)(s => assertTrue(UrlEncoding.decode(s).length >= 0)) &&
      assertTrue(UrlEncoding.decode("%zz%4") == "%zz%4", UrlEncoding.decode("a%2Fb") == "a/b")
    ,
    test("parse inverts render for absolute URLs"):
      check(absolute)(url => assertTrue(Url.parse(url.render) == url))
    ,
    test("strict decode accepts every rendered URL"):
      check(absolute)(url => assertTrue(Url.decode(url.render) == Right(url)))
    ,
    test("parse is total"):
      check(anyText)(s => assertTrue(Url.parse(s).path.segments.length >= 0))
    ,
    test("strict decode names the first character no URI may hold"):
      val forbidden = (c: Char) => c == ' ' || c == '"' || c == '<' || c > '~'
      check(anyText.filter(_.exists(forbidden))) { s =>
        Url.decode(s) match
          case Left(UrlError.BadCharacter(c, at)) =>
            assertTrue(s.lift(at).contains(c), at <= s.indexWhere(forbidden))
          case other => assertNever(s"expected BadCharacter, got $other")
      }
    ,
    test("strict decode rejects a bad host or port"):
      assertTrue(
        Url.decode("http://exam ple.com") == Left(UrlError.BadCharacter(' ', 11)),
        Url.decode("http://example.com:99999/") == Left(UrlError.BadAuthority("example.com:99999")),
        Url.decode("http://example.com:8o/") == Left(UrlError.BadAuthority("example.com:8o")),
        Url.decode("http:///nohost") == Left(UrlError.BadAuthority("")),
        Url.decode("gopher://example.com/") == Left(UrlError.UnknownScheme("gopher")),
        Url.decode("") == Left(UrlError.Empty),
      ),
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(60))
end UrlLawsSpec
