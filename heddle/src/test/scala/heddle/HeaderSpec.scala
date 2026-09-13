package heddle

import zio.Chunk
import zio.test.*

object HeaderSpec extends ZIOSpecDefault:
  def spec =
    suite("Headers")(
      suite("HeaderName")(
        test("mixed-case Content-Type bytes intern to the catalog singleton"):
          val raw = "content-type".getBytes
          assertTrue(HeaderName.intern(raw, 0, raw.length) eq HeaderName.ContentType)
        ,
        test("apply intern-hits any ASCII case"):
          assertTrue(
            HeaderName("content-type") eq HeaderName.ContentType,
            HeaderName("CONTENT-TYPE") eq HeaderName.ContentType,
          )
        ,
        test(":protocol is not a catalog name"):
          assertTrue(
            !(HeaderName(":protocol") eq HeaderName.ContentType),
            HeaderName(":protocol").render == ":protocol",
          )
        ,
        test("unknown names of equal length are not globally interned"):
          val a = HeaderName("X-Totally-Unknown-Header-Name-XXXX")
          val b = HeaderName("X-Totally-Unknown-Header-Name-XXXX")
          assertTrue(!(a eq b), a == b)
        ,
        test("get is case-insensitive"):
          val hs = Headers.empty.add(HeaderName.ContentType, "text/plain")
          assertTrue(
            hs.get("content-type").contains("text/plain"),
            hs.get(HeaderName.ContentType).contains("text/plain"),
          ),
      ),
      suite("MediaType")(
        test("catalog constants round-trip parse(render)"):
          val types = List(
            MediaType.Json,
            MediaType.JsonUtf8,
            MediaType.TextUtf8,
            MediaType.HtmlUtf8,
            MediaType.EventStream,
            MediaType.OctetStream,
          )
          assertTrue(types.forall(t => MediaType.parse(t.render).contains(t)))
        ,
        test("Json is not JsonUtf8"):
          assertTrue(MediaType.Json != MediaType.JsonUtf8)
        ,
        test("parse Application/JSON is Json"):
          assertTrue(MediaType.parse("Application/JSON").contains(MediaType.Json))
        ,
        test("parse quoted charset equals JsonUtf8"):
          assertTrue(MediaType.parse("""application/json; charset="utf-8"""").contains(MediaType.JsonUtf8))
        ,
        test("missing slash is None"):
          assertTrue(MediaType.parse("json").isEmpty, MediaType.parse("").isEmpty),
      ),
      suite("structured headers")(
        test("cookies flatten two Cookie headers and unquote"):
          val hs = Headers.empty
            .add(HeaderName.Cookie, "a=1")
            .add(HeaderName.Cookie, """session="abc"; flag""")
          assertTrue(
            hs.cookies == Chunk(CookiePair("a", "1"), CookiePair("session", "abc"), CookiePair("flag", ""))
          )
        ,
        test("Set-Cookie ignores unknown attrs and fails dates open"):
          val raw = """id=x; Partitioned; Expires=not-a-date; Max-Age=nope; Secure; SameSite=Lax"""
          val sc  = SetCookie.parse(raw).get
          assertTrue(
            sc.name == "id",
            sc.value == "x",
            sc.secure,
            sc.sameSite.contains(SameSite.Lax),
            sc.expires.isEmpty,
            sc.maxAge.isEmpty,
          )
        ,
        test("Authorization Bearer Basic Other and missing space"):
          assertTrue(
            Authorization.parse("Bearer abc") == Authorization(AuthScheme.Bearer, "abc"),
            Authorization.parse("Basic n:m") == Authorization(AuthScheme.Basic, "n:m"),
            Authorization.parse("Token xyz") == Authorization(AuthScheme.Other("Token"), "xyz"),
            Authorization.parse("Bearer") == Authorization(AuthScheme.Bearer, ""),
            Authorization.parse("bearer tok").scheme == AuthScheme.Bearer,
          )
        ,
        test("WWW-Authenticate strips quoted realm"):
          val w = WwwAuthenticate.parse("""Bearer realm="foo", charset=UTF-8""")
          assertTrue(
            w.scheme == AuthScheme.Bearer,
            w.params.contains("realm"   -> "foo"),
            w.params.contains("charset" -> "UTF-8"),
          )
        ,
        test("Host with port and IPv6 brackets"):
          assertTrue(
            Host.parse("example.com:8080").contains(Host("example.com", Some(8080))),
            Host.parse("[::1]:443").contains(Host("::1", Some(443))),
            Host.parse("localhost").contains(Host("localhost", None)),
          )
        ,
        test("Location illegal absolute URI is None"):
          val hs = Headers.empty.add(HeaderName.Location, "http://exam ple.com")
          assertTrue(hs.location.isEmpty)
        ,
        test("Set-Cookie render round-trips SameSite Secure HttpOnly"):
          val sc =
            SetCookie("sid", "abc", path = Some("/"), secure = true, httpOnly = true, sameSite = Some(SameSite.Lax))
          val raw  = SetCookie.render(sc)
          val back = SetCookie.parse(raw).get
          assertTrue(
            raw.contains("sid=abc"),
            raw.contains("Secure"),
            raw.contains("HttpOnly"),
            raw.contains("SameSite=Lax"),
            back == sc.copy(path = Some("/")),
          )
        ,
        test("BasicCredentials round-trip and reject junk"):
          val raw = BasicCredentials.render("ada", "s3:cret")
          assertTrue(
            BasicCredentials.parse(raw).contains(BasicCredentials("ada", "s3:cret")),
            BasicCredentials.parse("%%%").isEmpty,
            Authorization.basic("ada", "pw").scheme == AuthScheme.Basic,
          )
        ,
        test("Date RFC 1123 junk is None"):
          val ok  = Headers.empty.add(HeaderName.Date, "Sun, 06 Nov 1994 08:49:37 GMT")
          val bad = Headers.empty.add(HeaderName.Date, "not-a-date")
          assertTrue(ok.date.isDefined, bad.date.isEmpty),
      ),
    )
end HeaderSpec
