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
      test("multipart parse inverts encode for any names, filenames, and values"):
        val oneLine = Gen.string.map(_.filterNot(c => c == '\r' || c == '\n'))
        val name    = oneLine.filter(_.nonEmpty)
        val field   = Gen.oneOf(
          (name <*> Gen.string).map((n, v) => FormField.Text(n, v)),
          (name <*> ByteGens.upTo(32) <*> oneLine).map((n, d, f) =>
            FormField.Binary(n, d, MediaType.OctetStream, Some(f))
          ),
        )
        check(Gen.chunkOfBounded(1, 4)(field)) { fields =>
          assertTrue(Multipart.parse(Multipart.encode(fields, "----law"), "----law") == Right(fields))
        }
      ,
      test("a part's name is its name parameter, wherever filename sits"):
        val raw = "--b\r\nContent-Disposition: form-data; filename=\"a.txt\"; name=\"file\"\r\n\r\nabc\r\n--b--\r\n"
        assertTrue(
          Multipart
            .parse(Chunk.fromArray(raw.getBytes), "b")
            .map(_.map {
              case FormField.Binary(n, _, _, f) => n -> f
              case FormField.Text(n, _, _)      => n -> None
            }) == Right(Chunk("file" -> Some("a.txt")))
        )
      ,
      test("multipart failures are typed"):
        val open = "--b\r\nContent-Disposition: form-data; name=\"x\"\r\n\r\nabc"
        assertTrue(
          Multipart.parse(Chunk.fromArray("no boundary here".getBytes), "b") == Left(MultipartError.NoBoundary),
          Multipart.parse(Chunk.fromArray(open.getBytes), "b") == Left(MultipartError.Truncated),
        )
      ,
      test("a body without a declared boundary is Undeclared"):
        Body.fromBytes(Chunk.fromArray("x".getBytes), Some(MediaType.MultipartForm)).asMultipart.either.map { r =>
          assertTrue(r == Left(MultipartError.Undeclared))
        }
      ,
      test("Endpoint.inForm decodes the body"):
        val ep     = Endpoint.post("f").inForm.outText()
        val routes = ep.implement(form => ZIO.succeed(form.get("n").getOrElse("")))
        routes(Request.post("/f", Body.form(Form("n" -> "ada")))).map { res =>
          assertTrue(res.body.text.is(_.some) == "ada")
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
    ) @@ TestAspect.timeout(60.seconds)
end FormSpec
