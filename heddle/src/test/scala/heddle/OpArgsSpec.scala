package heddle

import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

object OpArgsSpec extends ZIOSpecDefault:
  given JsonCodec[Item]    = JsonCodec.from(_ => """{"id":1,"name":"a"}""", _ => Right(Item(1, "a")))
  given JsonCodec[NewItem] = JsonCodec.from(n => s"""{"name":"${n.name}"}""", _ => Right(NewItem("a")))

  def spec =
    suite("OpArgs")(
      test("flattens path, query, and JSON body fields"):
        val ep = Endpoint
          .post("items" / int("id"))
          .query[String]("q")
          .inJson[NewItem]
          .out[Item]
        OpArgs.inputSchema(ep.doc) match
          case Left(err)                                    => assertTrue(err.isEmpty)
          case Right(SchemaDoc.Object(_, fields, required)) =>
            assertTrue(
              fields.map(_.name) == List("id", "q", "name"),
              required.contains("id"),
              required.contains("q"),
              required.contains("name"),
            )
          case Right(_) => assertTrue(false)
        end match
      ,
      test("fails on path and body name collision"):
        val ep = Endpoint.post("items" / int("name")).inJson[NewItem].out[Item]
        OpArgs.inputSchema(ep.doc) match
          case Left(msg) => assertTrue(msg.contains("collision"), msg.contains("name"))
          case Right(_)  => assertTrue(false)
      ,
      test("nestBody keeps the body as one property"):
        val ep = Endpoint.post("items" / int("name")).inJson[NewItem].out[Item].nestBody
        OpArgs.inputSchema(ep.doc) match
          case Right(SchemaDoc.Object(_, fields, _)) =>
            assertTrue(fields.map(_.name) == List("name", "NewItem"))
          case _ => assertTrue(false)
      ,
      test("empty input schema is a closed object"):
        val ep = Endpoint.get("items").out[Item]
        OpArgs.inputSchema(ep.doc).map(_.jsonSchema.toJson) match
          case Right(json) =>
            assertTrue(json.contains("\"additionalProperties\":false"), json.contains("\"type\":\"object\""))
          case Left(_) => assertTrue(false)
      ,
      test("request builds path, query, and body"):
        val ep = Endpoint
          .post("items" / int("id"))
          .query[String]("q")
          .inJson[NewItem]
          .out[Item]
        val args = Json.Obj(
          "id"   -> Json.Num(7),
          "q"    -> Json.Str("x"),
          "name" -> Json.Str("ada"),
        )
        OpArgs.request(ep.doc, args) match
          case Left(err)  => assertTrue(err.isEmpty)
          case Right(req) =>
            assertTrue(
              req.method == Method.POST,
              req.path.render == "/items/7",
              req.query.get("q").contains("x"),
              req.body.asString.contains("ada"),
            )
      ,
      test("GET with JSON out is promotable; SSE is not"):
        val json = Endpoint.get("items").out[Item]
        val sse  = Endpoint.get("stream").outSse
        assertTrue(OpArgs.promotable(json.doc), !OpArgs.promotable(sse.doc)),
    ) @@ TestAspect.timeout(5.seconds)
end OpArgsSpec
