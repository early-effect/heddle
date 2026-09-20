package heddle

import zio.test.*

object UrlEncodingSpec extends ZIOSpecDefault:
  def spec =
    suite("UrlEncoding")(
      test("RFC 3986 keeps unreserved and percent-encodes the rest"):
        assertTrue(
          UrlEncoding.encode("abc") == "abc",
          UrlEncoding.encode("a b") == "a%20b",
          UrlEncoding.encode("a+b") == "a%2Bb",
          UrlEncoding.encode("~*") == "~%2A",
          UrlEncoding.encode("é") == "%C3%A9",
          UrlEncoding.encode("€") == "%E2%82%AC",
          UrlEncoding.decode("a%20b") == "a b",
          UrlEncoding.decode("a+b") == "a+b",
          UrlEncoding.decode("%C3%A9") == "é",
          UrlEncoding.decode("%E2%82%AC") == "€",
        )
      ,
      test("form-urlencoded matches URLEncoder space and star rules"):
        assertTrue(
          UrlEncoding.encodeForm("a b") == "a+b",
          UrlEncoding.encodeForm("1&2") == "1%262",
          UrlEncoding.encodeForm("*~") == "*%7E",
          UrlEncoding.encodeForm("é") == "%C3%A9",
          UrlEncoding.decodeForm("a+b") == "a b",
          UrlEncoding.decodeForm("1%262") == "1&2",
          UrlEncoding.decodeForm("*%7E") == "*~",
        )
      ,
      test("form round-trip through Form"):
        val form = Form("q" -> "a b", "x" -> "1&2", "c" -> "€")
        val back = Form.decode(form.render)
        assertTrue(
          back.get("q").contains("a b"),
          back.get("x").contains("1&2"),
          back.get("c").contains("€"),
        ),
    )
end UrlEncodingSpec
