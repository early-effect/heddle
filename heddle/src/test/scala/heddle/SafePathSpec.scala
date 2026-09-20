package heddle

import heddle.http.Path
import heddle.server.SafePath
import zio.test.*

object SafePathSpec extends ZIOSpecDefault:
  def spec =
    suite("SafePath")(
      test("remainder keeps nested files under the prefix"):
        val got = SafePath.remainder("/static", Path.decode("/static/css/app.css"))
        assertTrue(got.contains(List("css", "app.css")))
      ,
      test("remainder rejects parent segments"):
        val escaped = SafePath.remainder("/static", Path.decode("/static/../secret.txt"))
        assertTrue(escaped.isEmpty)
      ,
      test("remainder rejects encoded parent segments"):
        val escaped = SafePath.remainder("/static", Path.decode("/static/%2e%2e/secret.txt"))
        assertTrue(escaped.isEmpty)
      ,
      test("remainder rejects a prefix that is only a string prefix"):
        val got = SafePath.remainder("/static", Path.decode("/staticX/file.txt"))
        assertTrue(got.isEmpty)
      ,
      test("resolveUnder rejects .. in the relative path"):
        val got = SafePath.resolveUnder("/var/www", "../etc/passwd")
        assertTrue(got.isEmpty)
      ,
      test("resolveUnder rejects backslash traversal"):
        val got = SafePath.resolveUnder("/var/www", "..\\etc\\passwd")
        assertTrue(got.isEmpty)
      ,
      test("resolveUnder rejects NUL"):
        val got = SafePath.resolveUnder("/var/www", "ok\u0000/../secret")
        assertTrue(got.isEmpty)
      ,
      test("resolveUnder rejects a root that already contains .."):
        val got = SafePath.resolveUnder("/var/www/../etc", "passwd")
        assertTrue(got.isEmpty)
      ,
      test("resolveUnder joins a clean relative path"):
        val got = SafePath.resolveUnder("/var/www", "css/app.css")
        assertTrue(got.contains("/var/www/css/app.css"))
      ,
      test("resolveUnder treats empty relative as the root"):
        val got = SafePath.resolveUnder("/var/www", "")
        assertTrue(got.contains("/var/www")),
    )
end SafePathSpec
