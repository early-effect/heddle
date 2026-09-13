package heddle

import zio.*
import zio.test.*

final case class Book(title: String, pages: Int) derives Schema

object OpenApiSpec extends ZIOSpecDefault:
  given JsonCodec[String] = JsonCodec.from(identity, Right(_))
  given JsonCodec[Book]   =
    JsonCodec.from(_ => "", _ => Left("unused"))

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
        ),
    ) @@ TestAspect.timeout(5.seconds)
end OpenApiSpec
