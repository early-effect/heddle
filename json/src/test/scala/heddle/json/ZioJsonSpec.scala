package heddle.json

import heddle.*
import heddle.json.given
import zio.*
import zio.json.{JsonDecoder, JsonEncoder}
import zio.test.*

final case class Item(id: Int, label: String) derives Schema, JsonEncoder, JsonDecoder

object ZioJsonSpec extends ZIOSpecDefault:
  def spec =
    suite("heddle-zio-json")(
      test("round-trips a case class through an endpoint on the loom server"):
        val create =
          Endpoint.post("items").inJson[Item].out[Item](Status.Created)
        val routes = create.implement(item => ZIO.succeed(item))
        LiveServer(routes) { base =>
          val json = """{"id":3,"label":"bolt"}"""
          Client.request(Method.POST, s"$base/items", body = Body.json(json)).map { res =>
            assertTrue(
              res.status == Status.Created,
              res.body.asString.contains("\"id\":3"),
              res.body.asString.contains("\"label\":\"bolt\""),
            )
          }
        }
    ) @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock
end ZioJsonSpec
