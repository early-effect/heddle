package heddle.brotli

import java.io.ByteArrayInputStream
import org.brotli.dec.BrotliInputStream
import heddle.*
import zio.*
import zio.stream.ZStream
import zio.test.*

object BrotliSpec extends ZIOSpecDefault:
  def spec =
    suite("Brotli")(
      test("round-trips JSON against org.brotli.dec"):
        val raw = Chunk.fromArray(("n" * 2048).getBytes)
        val enc = Brotli.encode(raw)
        val dec = oracle(enc)
        assertTrue(enc.nonEmpty, enc.length < raw.length / 4, dec.toArray.toSeq == raw.toArray.toSeq)
      ,
      test("round-trips mixed bytes"):
        val raw = Chunk.fromArray((0 until 512).map(i => (i * 13).toByte).toArray)
        val enc = Brotli.encode(raw)
        assertTrue(oracle(enc).toArray.toSeq == raw.toArray.toSeq)
      ,
      test("empty input round-trips"):
        val enc = Brotli.encode(Chunk.empty)
        assertTrue(oracle(enc).isEmpty)
      ,
      test("single byte round-trips"):
        val raw = Chunk.single(0.toByte)
        assertTrue(oracle(Brotli.encode(raw)).toArray.toSeq == raw.toArray.toSeq)
      ,
      test("QRST at 0 and 65521 round-trips through org.brotli.dec"):
        val raw = Array.fill(65521 + 4)(0.toByte)
        raw(0) = 'Q'; raw(1) = 'R'; raw(2) = 'S'; raw(3) = 'T'
        raw(65521) = 'Q'; raw(65522) = 'R'; raw(65523) = 'S'; raw(65524) = 'T'
        val enc = Brotli.encode(Chunk.fromArray(raw))
        val dec = oracle(enc)
        assertTrue(dec.toArray.toSeq == raw.toSeq)
      ,
      test("encode then org.brotli.dec is identity"):
        check(BrotliGens.bytes) { raw =>
          val enc = Brotli.encode(Chunk.fromArray(raw))
          assertTrue(oracle(enc).toArray.toSeq == raw.toSeq)
        }
      ,
      test("encode then our decode is identity"):
        check(BrotliGens.bytes) { raw =>
          val enc = Brotli.encode(Chunk.fromArray(raw))
          assertTrue(Brotli.decode(enc).toArray.toSeq == raw.toSeq)
        }
      ,
      test("streamed encode then org.brotli.dec is identity"):
        check(BrotliGens.bytes) { raw =>
          Brotli.compressor.stream(ZStream.fromIterable(raw)).runCollect.map { enc =>
            assertTrue(oracle(enc).toArray.toSeq == raw.toSeq)
          }
        }
      ,
      test("our decode of Google brotli fixtures"):
        val names = List("empty", "hello", "zeros", "ns", "mixed", "json")
        val bad   = names.flatMap { name =>
          val raw = resource(s"/heddle/brotli/google/$name.bin")
          val br  = resource(s"/heddle/brotli/google/$name.br")
          val got = Brotli.decode(Chunk.fromArray(br)).toArray
          if got.toSeq == raw.toSeq then Nil else List(name)
        }
        assertTrue(bad.isEmpty)
      ,
      test("repeated input shrinks"):
        check(BrotliGens.compressible) { raw =>
          val enc = Brotli.encode(Chunk.fromArray(raw))
          assertTrue(enc.length < raw.length)
        }
      ,
      test("middleware sets Content-Encoding br"):
        val routes =
          Routes(Method.GET / "j" -> Handler.text("n" * 2048)) @@
            Middleware.compress(minBytes = 16, compressors = Chunk(Brotli.compressor))
        routes(Request.get("/j").withHeader("Accept-Encoding", "br")).map { res =>
          val dec = oracle(res.body.asBytes)
          assertTrue(
            res.header("Content-Encoding").contains("br"),
            dec.toArray.toSeq == ("n" * 2048).getBytes.toSeq,
          )
        }
      ,
      test("decompress middleware recovers a br request body"):
        val raw    = "hello heddle brotli".getBytes
        val enc    = Brotli.encode(Chunk.fromArray(raw))
        val routes =
          Routes(
            Method.POST / "echo" -> Handler.fromFunctionZIO((req: Request) =>
              ZIO.succeed(Response.text(req.body.asString))
            )
          ) @@
            Middleware.decompress(Chunk(Brotli.decompressor))
        val req = Request.post("/echo", Body.fromBytes(enc)).withHeader("Content-Encoding", "br")
        routes(req).map { res =>
          assertTrue(res.body.asString == "hello heddle brotli")
        },
    ) @@ TestAspect.timeout(60.seconds)

  private def oracle(bytes: Chunk[Byte]): Chunk[Byte] =
    val in = BrotliInputStream(ByteArrayInputStream(bytes.toArray))
    try Chunk.fromArray(in.readAllBytes())
    finally in.close()

  private def resource(path: String): Array[Byte] =
    val in = getClass.getResourceAsStream(path)
    try in.readAllBytes()
    finally in.close()
end BrotliSpec
