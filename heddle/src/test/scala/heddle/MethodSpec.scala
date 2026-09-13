package heddle

import zio.Chunk
import zio.test.*

object MethodSpec extends ZIOSpecDefault:
  def spec =
    suite("Method")(
      test("CONNECT bytes parse to CONNECT"):
        val raw = Chunk.fromArray("CONNECT".getBytes)
        assertTrue(Method.parse(raw, 0, raw.length).contains(Method.CONNECT))
      ,
      test("lowercase get parses as GET"):
        val raw = Chunk.fromArray("get".getBytes)
        assertTrue(Method.parse(raw, 0, raw.length).contains(Method.GET), Method.parse("get").contains(Method.GET))
      ,
      test("Custom keeps the token"):
        assertTrue(
          Method.parse("PROPFIND").contains(Method.Custom("PROPFIND")),
          Method.Custom("PROPFIND").render == "PROPFIND",
        )
      ,
      test("empty parse is None"):
        assertTrue(Method.parse("").isEmpty, Method.parse(Chunk.empty, 0, 0).isEmpty)
      ,
      test("HTTP/1.1 CONNECT on a GET-only path is 405 with Allow GET"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request(Method.CONNECT, Url.parse("/health"))).map { res =>
          assertTrue(res.status == Status.MethodNotAllowed, res.header("Allow").contains("GET"))
        }
      ,
      test("HTTP/1.1 CONNECT on an unknown path is 404"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        routes(Request(Method.CONNECT, Url.parse("/nope"))).map { res =>
          assertTrue(res.status == Status.NotFound)
        }
      ,
      test("CONNECT route lists CONNECT in Allow"):
        val routes =
          Routes(Method.GET / "x" -> Handler.text("g")) ++
            Routes(Method.CONNECT / "x" -> Handler.text("c"))
        routes(Request(Method.POST, Url.parse("/x"))).map { res =>
          val allow = res.header("Allow").getOrElse("")
          assertTrue(res.status == Status.MethodNotAllowed, allow.contains("GET"), allow.contains("CONNECT"))
        },
    )
end MethodSpec
