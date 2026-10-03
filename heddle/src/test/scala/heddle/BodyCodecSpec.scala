package heddle

import heddle.http.header.HeaderName
import zio.*
import zio.test.*

final case class Note(title: String) derives Schema, zio.json.JsonCodec

object Note:
  given BodyCodec[Note] =
    BodyCodec.text[Note](MediaType("text", "csv", Some("utf-8")))(_.title, Note(_))

opaque type Csv = String

object Csv:
  def apply(value: String): Csv          = value
  extension (csv: Csv) def value: String = csv
  given BodyCodec[Csv]                   =
    BodyCodec.text[Csv](MediaType("text", "csv", Some("utf-8")))(identity, Csv.apply)

object BodyCodecSpec extends ZIOSpecDefault:
  def spec =
    suite("BodyCodec")(
      test("a domain type is JSON unless its companion gives another codec"):
        val note   = Endpoint.get("note").out[Note]
        val item   = Endpoint.get("item").out[Item]
        val routes = note.implement(_ => ZIO.succeed(Note("ada")))
        routes(Request.get("/note")).map { res =>
          assertTrue(
            res.header(HeaderName.ContentType).contains("text/csv; charset=utf-8"),
            res.body.text.contains("ada"),
            !res.body.text.exists(_.startsWith("{")),
            item.doc.responses.exists(_.contentType.contains(MediaType.Json)),
            !OpArgs.promotable(note.doc),
            OpArgs.promotable(item.doc),
          )
        }
      ,
      test("HTML is its own type, including an empty document"):
        val ep     = Endpoint.get(PathCodec.empty).out[Html]
        val routes = ep.implement(_ => ZIO.succeed(Html("")))
        for
          res  <- routes(Request.get("/"))
          back <- ep.fromResponse(res)
        yield assertTrue(
          res.header(HeaderName.ContentType).contains(MediaType.HtmlUtf8.render),
          back == Html(""),
          ep.doc.responses.exists(_.contentType.contains(MediaType.HtmlUtf8)),
        )
      ,
      test("in and out share the HTML codec"):
        val ep     = Endpoint.post("page").in[Html].out[Html]
        val routes = ep.implement(body => ZIO.succeed(body))
        val req    = ep.toRequest(Html("<p>ada</p>"), Url.root)
        for
          res <- routes(req)
          out <- ep.fromResponse(res)
        yield assertTrue(
          req.body.mediaType.contains(MediaType.HtmlUtf8),
          res.header(HeaderName.ContentType).contains(MediaType.HtmlUtf8.render),
          out == Html("<p>ada</p>"),
        )
      ,
      test("an application codec is not JSON"):
        val ep     = Endpoint.get("rows").out[Csv]
        val routes = ep.implement(_ => ZIO.succeed(Csv("a,b")))
        routes(Request.get("/rows")).map { res =>
          assertTrue(
            res.header(HeaderName.ContentType).contains("text/csv; charset=utf-8"),
            res.body.text.contains("a,b"),
            !OpArgs.promotable(ep.doc),
          )
        }
      ,
      test("a charset other than utf-8 fails decode"):
        val codec = BodyCodec.text[String](MediaType("text", "plain", Some("iso-8859-1")))(identity, identity)
        codec.decode(Body.text("hi")).either.map { got =>
          assertTrue(got == Left(BodyError.Charset("iso-8859-1")))
        }
      ,
      test("an HTML error is that media type and not a tool success"):
        val ep  = Endpoint.get("page").out[Html].outError[Html](Status.BadRequest)
        val res = ep.encodeErr(Html("<p>no</p>"))
        assertTrue(
          res.status == Status.BadRequest,
          res.header(HeaderName.ContentType).contains(MediaType.HtmlUtf8.render),
          res.body.text.contains("<p>no</p>"),
          ep.errors.json(Html("<p>no</p>")).isEmpty,
          !OpArgs.promotable(ep.doc),
        )
      ,
      test("out[Unit] and a bare byte chunk have no codec"):
        for
          unit  <- typeCheck("""Endpoint.get("gone").out[Unit]""")
          bytes <- typeCheck("""Endpoint.get("bin").out[zio.Chunk[Byte]]""")
        yield assertTrue(unit.isLeft, bytes.isLeft),
    ) @@ TestAspect.timeout(5.seconds)
end BodyCodecSpec
