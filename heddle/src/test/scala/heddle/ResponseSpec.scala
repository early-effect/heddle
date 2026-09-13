package heddle

import zio.*
import zio.test.*

object ResponseSpec extends ZIOSpecDefault:
  def spec =
    suite("Response")(
      test("equals and hashCode ignore ws"):
        val plain = Response.ok
        val ws    = Response.withWs(Status.Ok, Headers.empty, (_, _) => ZIO.unit)
        assertTrue(plain == ws, plain.hashCode == ws.hashCode, ws.ws.isDefined, plain.ws.isEmpty)
      ,
      test("copy keeps ws and replaces headers"):
        val ws  = Response.withWs(Status.SwitchingProtocols, Headers.empty, (_, _) => ZIO.unit)
        val cpy = ws.copy(headers = ws.headers.add("X-A", "1"))
        assertTrue(
          cpy.ws.isDefined,
          cpy.status == Status.SwitchingProtocols,
          cpy.headers.get("X-A").contains("1"),
        )
      ,
      test("apply does not attach a websocket"):
        assertTrue(Response(Status.Ok).ws.isEmpty, Response.ok.ws.isEmpty),
    ) @@ TestAspect.timeout(5.seconds)
end ResponseSpec
