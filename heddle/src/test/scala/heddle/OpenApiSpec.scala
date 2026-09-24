package heddle

import zio.*
import zio.test.*

import zio.json.*
import zio.json.ast.Json

final case class Book(title: String, pages: Int) derives Schema, JsonCodec

object OpenApiSpec extends ZIOSpecDefault:

  def spec =
    suite("OpenApi")(
      test("emits paths, path params, and component schemas"):
        val getBook = Endpoint.get("books" / int("id")).out[Book].name("getBook").summary("Get a book")
        val json    = OpenApi.from("Library", "1.0.0", getBook).toJson
        assertTrue(
          json.contains("\"openapi\":\"3.1.0\""),
          json.contains("\"/books/{id}\""),
          json.contains("\"get\""),
          json.contains("\"getBook\""),
          json.contains("\"#/components/schemas/Book\""),
          json.contains("\"title\""),
          json.contains("\"pages\""),
        )
      ,
      test("swagger routes serve HTML and the spec"):
        val ep   = Endpoint.get("ping").outText()
        val spec = OpenApi.from("Ping", "0.0.1", ep)
        val app  = spec.routes("docs")
        for
          html <- app(Request.get("/docs"))
          json <- app(Request.get("/docs/openapi.json"))
        yield assertTrue(
          html.header("Content-Type").exists(_.contains("text/html")),
          html.body.asString.contains("swagger-ui"),
          json.body.asString.contains("\"/ping\""),
        )
      ,
      test("documents query params, json bodies, tags, and errors"):
        val create = Endpoint
          .post("books")
          .query[String]("q")
          .inJson[Book]
          .out[Book](Status.Created)
          .outError[String](Status.BadRequest)
          .tag("books")
          .summary("Create a book")
        val json = OpenApi.from("Library", "1.0.0", create).toJson
        assertTrue(
          json.contains("\"/books\""),
          json.contains("\"post\""),
          json.contains("\"q\""),
          json.contains("\"in\":\"query\""),
          json.contains("\"application/json\""),
          json.contains("\"books\""),
          json.contains("\"201\""),
          json.contains("\"400\""),
        )
      ,
      test("outErrors documents one response per status with that case's wire schema"):
        val order = errorSchema(ErrorFixtures.order, "/orders/{id}", "post")
        assertTrue(
          order("404")
            .flatMap(wrapped)
            .contains("NotFound" -> Json.Obj("$ref" -> Json.Str("#/components/schemas/NotFound"))),
          order("409").flatMap(wrapped).map(_._1).contains("Conflict"),
          order("503").flatMap(wrapped).map(_._1).contains("Unavailable"),
          order.keySet == Set("200", "404", "409", "503"),
        )
      ,
      test("cases sharing a status document a oneOf of their wire schemas"):
        val seating = errorSchema(ErrorFixtures.seating, "/seats", "post")
        val labels  = seating("409").collect { case o: Json.Obj => o.get("oneOf") }.flatten match
          case Some(Json.Arr(variants)) => variants.toList.flatMap(wrapped).map(_._1)
          case _                        => Nil
        assertTrue(labels == List("SoldOut", "NoBlock"), seating("410").flatMap(wrapped).map(_._1).contains("Closed"))
      ,
      test("an all-singleton enum documents each status as a one-value string enum"):
        val light = errorSchema(ErrorFixtures.light, "/light", "get")
        assertTrue(
          light("403").contains(Json.Obj("type" -> Json.Str("string"), "enum" -> Json.Arr(Json.Str("Red")))),
          light("410").contains(Json.Obj("type" -> Json.Str("string"), "enum" -> Json.Arr(Json.Str("Green")))),
        ),
    ) @@ TestAspect.timeout(5.seconds)

  private def errorSchema(ep: Endpoint[?, ?, ?], path: String, method: String): Map[String, Option[Json]] =
    val root      = OpenApi.from("Errors", "1", ep).toJson.fromJson[Json].toOption
    val responses = root.flatMap(at(_, "paths", path, method, "responses")).collect { case o: Json.Obj => o }
    responses.toList.flatMap(_.fields).map((code, r) => code -> at(r, "content", "application/json", "schema")).toMap

  private def at(json: Json, keys: String*): Option[Json] =
    keys.foldLeft(Option(json)) {
      case (Some(o: Json.Obj), k) => o.get(k)
      case _                      => None
    }

  private def wrapped(schema: Json): Option[(String, Json)] =
    at(schema, "properties").collect { case o: Json.Obj => o.fields.toList } match
      case Some(List((label, inner))) if at(schema, "required").contains(Json.Arr(Json.Str(label))) =>
        Some(label -> inner)
      case _ => None
end OpenApiSpec
