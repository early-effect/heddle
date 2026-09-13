package heddle

import zio.*
import zio.test.*

object FormSpec extends ZIOSpecDefault:
  def spec =
    suite("Form")(
      test("urlencoded round-trips spaces and amp"):
        val form = Form("q" -> "a b", "x" -> "1&2")
        val back = Form.decode(form.render)
        assertTrue(back.get("q").contains("a b"), back.get("x").contains("1&2"))
      ,
      test("Body.form is application/x-www-form-urlencoded"):
        val body = Body.form(Form("n" -> "ada"))
        body.asForm.map { form =>
          assertTrue(
            body.mediaType.contains(MediaType.FormUrlEncoded),
            form.get("n").contains("ada"),
          )
        }
      ,
      test("multipart round-trips text and file"):
        val boundary = "----testBoundary"
        val fields   = Chunk(
          FormField.Text("title", "hello"),
          FormField.Binary("file", Chunk.fromArray("abc".getBytes), MediaType.TextUtf8, Some("a.txt")),
        )
        val body = Body.multipart(fields, boundary)
        body.asMultipart.map { out =>
          assertTrue(
            body.mediaType.exists(_.params.contains("boundary" -> boundary)),
            out.exists {
              case FormField.Text("title", "hello", _) => true
              case _                                   => false
            },
            out.exists {
              case FormField.Binary("file", data, _, Some("a.txt")) =>
                data.toArray.toSeq == "abc".getBytes.toSeq
              case _ => false
            },
          )
        }
      ,
      test("Endpoint.inForm decodes the body"):
        val ep     = Endpoint.post("f").inForm.outText()
        val routes = ep.implement(form => ZIO.succeed(form.get("n").getOrElse("")))
        routes(Request.post("/f", Body.form(Form("n" -> "ada")))).map { res =>
          assertTrue(res.body.asString == "ada")
        }
      ,
      test("RangeSpec parses suffix and open end"):
        assertTrue(
          RangeSpec.parse("bytes=0-499").contains(RangeSpec("bytes", Chunk(ByteRange.Inclusive(0, Some(499))))),
          RangeSpec.parse("bytes=500-").contains(RangeSpec("bytes", Chunk(ByteRange.Inclusive(500, None)))),
          RangeSpec.parse("bytes=-100").contains(RangeSpec("bytes", Chunk(ByteRange.Suffix(100)))),
        )
      ,
      test("EntityTag parses weak tags"):
        assertTrue(
          EntityTag.parse("""W/"abc"""").contains(EntityTag("abc", weak = true)),
          EntityTag.parse(""""abc"""").contains(EntityTag("abc", weak = false)),
        )
      ,
      test("Response.unauthorized sets WWW-Authenticate"):
        val res = Response.unauthorized()
        assertTrue(
          res.status == Status.Unauthorized,
          res.header("WWW-Authenticate").exists(_.startsWith("Bearer")),
        )
      ,
      test("Response.redirect is 302 with Location"):
        val res = Response.redirect("/docs")
        assertTrue(res.status == Status.Found, res.header("Location").contains("/docs"))
      ,
      test("Request.cookie reads Cookie header"):
        val req = Request.get("/").addCookie("sid", "abc")
        assertTrue(req.cookie("sid").contains("abc")),
    ) @@ TestAspect.timeout(5.seconds)
end FormSpec
