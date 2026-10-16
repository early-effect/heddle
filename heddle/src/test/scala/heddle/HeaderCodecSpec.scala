package heddle

import java.time.Instant
import heddle.http.header.HttpDate
import zio.*
import zio.test.*

object HeaderCodecSpec extends ZIOSpecDefault:
  private val sunday = Instant.parse("1994-11-06T08:49:37Z")

  def spec = suite("HeaderCodec")(
    test("int, long, and boolean match the query spellings"):
      assertTrue(
        HeaderCodec[Int].decode(Chunk.empty) == Left(ParamError.Missing),
        HeaderCodec[Int].decode(Chunk("x")) == Left(ParamError.Malformed("x", "an int")),
        HeaderCodec[Int].decode(Chunk("4", "9")) == Right(4),
        HeaderCodec[Int].encode(4) == Chunk("4"),
        HeaderCodec[Int].schema == SchemaDoc.Integer(Some("int32")),
        HeaderCodec[Long].decode(Chunk("7")) == Right(7L),
        HeaderCodec[Long].decode(Chunk("n")) == Left(ParamError.Malformed("n", "a long")),
        HeaderCodec[Long].schema == SchemaDoc.Integer(Some("int64")),
        HeaderCodec[Boolean].decode(Chunk("true")) == Right(true),
        HeaderCodec[Boolean].decode(Chunk("1")) == Right(true),
        HeaderCodec[Boolean].decode(Chunk("false")) == Right(false),
        HeaderCodec[Boolean].decode(Chunk("0")) == Right(false),
        HeaderCodec[Boolean].decode(Chunk("yes")) == Left(ParamError.Malformed("yes", "true, false, 1, or 0")),
        HeaderCodec[Boolean].schema == SchemaDoc.Boolean,
      )
    ,
    test("an optional int is absent, refuses a bad value, and omits None"):
      val codec = HeaderCodec[Option[Int]]
      assertTrue(
        codec.decode(Chunk.empty) == Right(None),
        codec.decode(Chunk("x")) == Left(ParamError.Malformed("x", "an int")),
        codec.encode(None) == Chunk.empty,
        codec.encode(Some(2)) == Chunk("2"),
        !codec.required,
      )
    ,
    test("an instant is an HTTP-date and renders IMF-fixdate"):
      val codec = HeaderCodec[Instant]
      assertTrue(
        codec.decode(Chunk("Sun, 06 Nov 1994 08:49:37 GMT")) == Right(sunday),
        codec.decode(Chunk("not-a-date")) == Left(ParamError.Malformed("not-a-date", "an HTTP-date")),
        codec.encode(sunday) == Chunk(HttpDate.render(sunday)),
        codec.encode(sunday) == Chunk("Sun, 06 Nov 1994 08:49:37 GMT"),
      )
    ,
    test("a url header keeps UrlError"):
      HeaderCodec[Url].decode(Chunk("http://exam ple.com/")) match
        case Left(ParamError.Url(UrlError.BadCharacter(' ', at))) => assertTrue(at == 11)
        case other                                                => assertNever(other.toString)
    ,
    test("entity tags: a list reads commas, one ETag rejects a second line"):
      assertTrue(
        HeaderCodec[Chunk[EntityTag]].decode(Chunk("\"a\", W/\"b\"")) ==
          Right(Chunk(EntityTag("a"), EntityTag("b", weak = true))),
        TypedHeader[EntityTag].decode(Chunk("\"a\"", "\"b\"")) ==
          Left(ParamError.Malformed("\"a\", \"b\"", "one value")),
      )
    ,
    test("basic credentials reject a bearer value"):
      val basic = Authorization.basic("ada", "pw")
      assertTrue(
        TypedHeader[BasicCredentials].decode(TypedHeader[Authorization].encode(basic)) ==
          Right(BasicCredentials("ada", "pw")),
        TypedHeader[BasicCredentials].decode(Chunk("Bearer tok")).isLeft,
      )
    ,
    test("a bearer token rejects basic and an empty token"):
      assertTrue(
        TypedHeader[BearerToken].decode(Chunk("Bearer tok")) == Right(BearerToken("tok")),
        TypedHeader[BearerToken].decode(Chunk("Basic xxx")).isLeft,
        TypedHeader[BearerToken].decode(Chunk("Bearer")).isLeft,
      )
    ,
    test("set-cookie encodes one line each and fails a bad line the bag still drops"):
      val cookies = Chunk(SetCookie("a", "1"), SetCookie("b", "2"))
      val headers = Headers.empty.add(HeaderName.SetCookie, "a=1").add(HeaderName.SetCookie, "novalue")
      assertTrue(
        TypedHeader[Chunk[SetCookie]].encode(cookies) == cookies.map(SetCookie.render),
        TypedHeader[Chunk[SetCookie]].decode(Chunk("a=1", "novalue")) ==
          Left(ParamError.Malformed("novalue", "a set-cookie")),
        headers.setCookies.length == 1,
      ),
  )
end HeaderCodecSpec
