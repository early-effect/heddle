package heddle

import heddle.client.{Authority, Target}
import heddle.client.internal.{Framing, Http1Conn, Http1Exchange, IoFailure, Limits}
import heddle.error.WireError
import heddle.http.Scheme
import heddle.http.header.OriginDate
import heddle.internal.duplex.ByteConn
import heddle.internal.engine.{ConnBuf, Http1, Lifecycle}
import heddle.server.{ChunkSize, HeaderBytes}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicBoolean
import zio.*
import zio.test.*

object ChunkedFramingSpec extends ZIOSpecDefault:
  private val payload       = bytes("x" * (64 * 1024))
  private val target        = Target(Scheme.Http, Authority("localhost", 80), "/")
  private val limits        = Limits(Duration.Infinity, 128, payload.length.toLong)
  private val io: IoFailure = (authority, cause) => ClientError.Io(authority, cause)
  private val post          = bytes("POST /body HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n")
  private val next          = bytes("GET /next HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
  private val encoded       = bytes("10000\r\n") ++ payload ++ bytes("\r\n0\r\n\r\n")

  def spec = suite("chunked framing")(
    test("a repeated read effect drains buffered bytes before EOF"):
      val data = bytes("abcdefghijkl")
      for
        src <- source(Chunk(data), 16)
        read = src.takeUpTo(8)
        first  <- read
        second <- read
        ended  <- read
      yield assertTrue(first == data.take(8), second == data.drop(8), ended.isEmpty)
    ,
    test("a 64 KiB wire chunk emits bounded fragments from a 128-byte buffer"):
      for
        src    <- source(Chunk(encoded), 128)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, payload.length.toLong, 128)
      yield assertTrue(pieces.flatten == payload, pieces.size > 1, pieces.forall(c => c.nonEmpty && c.length <= 128))
    ,
    test("partial payload reads do not have to fill the buffer or the wire chunk"):
      val input = Chunk(bytes("5\r\na"), bytes("bc"), bytes("de\r\n0\r\n\r\n"))
      for
        src    <- source(input, 128)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, 5, 128)
      yield assertTrue(pieces.flatten == bytes("abcde"), pieces.map(_.length) == Chunk(1, 2, 2))
    ,
    test("every header and delimiter byte can arrive in a separate read"):
      val input = bytes("2\r\nab\r\n3\r\ncde\r\n0\r\n\r\n").map(Chunk.single(_))
      for
        src    <- source(input, 8)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, 5, 8)
      yield assertTrue(pieces.flatten == bytes("abcde"), pieces.forall(_.length == 1))
    ,
    test("extensions and trailers leave the pipelined bytes unread"):
      val framed = bytes("2;ext=yes\r\nab\r\n3\r\ncde\r\n0;end=yes\r\nX-Test: yes\r\n\r\nNEXT")
      for
        src    <- source(Chunk.fromIterator(framed.grouped(3)), 32)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, 5, 32)
        ended  <- src.readChunkedPiece(state, 5, 32)
        after  <- ZIO.suspendSucceed(src.takeExact(4))
      yield assertTrue(pieces.flatten == bytes("abcde"), ended.isEmpty, after == bytes("NEXT"))
    ,
    test("a line at its byte limit accepts a separately arriving CRLF"):
      for
        src    <- source(Chunk(bytes("1;abc=xy"), bytes("\r"), bytes("\na\r\n0\r\n\r\n")), 8)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, 1, 8)
      yield assertTrue(pieces.flatten == bytes("a"))
    ,
    test("a pending line delimiter survives a zero-byte transport read"):
      for
        src    <- source(Chunk(bytes("1;abc=xy\r"), Chunk.empty, bytes("\na\r\n0\r\n\r\n")), 16)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, 1, 8)
      yield assertTrue(pieces.flatten == bytes("a"))
    ,
    test("lines and delimiters also work through a one-byte connection buffer"):
      for
        src    <- source(Chunk(bytes("1;ext=yes\r\na\r\n0\r\nX: yes\r\n\r\n")), 1)
        state  <- Ref.make(ConnBuf.ChunkedState.initial)
        pieces <- collect(src, state, 1, 16)
      yield assertTrue(pieces.flatten == bytes("a"))
    ,
    test("every two-read split preserves chunks, extensions and trailers"):
      val wire = bytes("2;x=y\r\nab\r\n3\r\ncde\r\n0\r\nX: y\r\n\r\n")
      ZIO
        .foreach(0 to wire.length) { at =>
          for
            src    <- source(Chunk(wire.take(at), wire.drop(at)), 16)
            state  <- Ref.make(ConnBuf.ChunkedState.initial)
            pieces <- collect(src, state, 5, 16)
          yield pieces.flatten
        }
        .map(out => assertTrue(out.forall(_ == bytes("abcde"))))
    ,
    test("a line over its byte limit fails without waiting for EOF"):
      for
        src   <- source(Chunk(bytes("1;abc=xyz")), 8)
        state <- Ref.make(ConnBuf.ChunkedState.initial)
        out   <- src.readChunkedPiece(state, 1, 8).either
      yield assertTrue(out == Left(HttpError.Malformed(WireError.ChunkLineTooLong)))
    ,
    test("EOF in payload or the next chunk-size line is a truncated chunk"):
      ZIO
        .foreach(List("5\r\nabc", "6\r\nabc\r\n", "1\r\na\r\n", "1\r\na\r\n0\r"))(raw => decode(raw, 32, 32).either)
        .map(out => assertTrue(out.forall(_ == Left(HttpError.Malformed(WireError.TruncatedChunk)))))
    ,
    test("EOF or invalid bytes in the payload delimiter is missing CRLF"):
      ZIO
        .foreach(List("1\r\na", "1\r\na\r", "1\r\naX\n"))(raw => decode(raw, 32, 32).either)
        .map(out => assertTrue(out.forall(_ == Left(HttpError.Malformed(WireError.MissingChunkCrlf)))))
    ,
    test("zero chunks require the trailer section's final blank line"):
      ZIO
        .foreach(List("0\r\n", "0\r\nX: y\r\n"))(raw => decode(raw, 32, 32).either)
        .map(out => assertTrue(out.forall(_ == Left(HttpError.Malformed(WireError.TruncatedChunk)))))
    ,
    test("declared and aggregate body limits reject before reading an oversized payload"):
      ZIO
        .foreach(List("6\r\n", "3\r\nabc\r\n3\r\n"))(raw => decode(raw, 5, 32).either)
        .map(out => assertTrue(out.forall(_ == Left(HttpError.BodyTooLarge))))
    ,
    test("aggregate accounting cannot overflow Long"):
      for
        src   <- source(Chunk(bytes("2\r\nab\r\n0\r\n\r\n")), 32)
        state <- Ref.make[ConnBuf.ChunkedState](ConnBuf.ChunkedState.Size(Long.MaxValue - 1))
        out   <- src.readChunkedPiece(state, Long.MaxValue, 32).either
      yield assertTrue(out == Left(HttpError.BodyTooLarge))
    ,
    test("a declared Long-sized chunk still emits only the available fragment"):
      for
        src   <- source(Chunk(bytes("7fffffffffffffff\r\na")), 32)
        state <- Ref.make(ConnBuf.ChunkedState.initial)
        out   <- src.readChunkedPiece(state, Long.MaxValue, 32)
      yield assertTrue(out.contains(bytes("a")))
    ,
    test("a chunk-size token that overflows Long is rejected as a typed error"):
      decode("8000000000000000\r\n", Long.MaxValue, 32).either.map(out =>
        assertTrue(out == Left(HttpError.Malformed(WireError.BadChunkSize("8000000000000000"))))
      )
    ,
    test("server request streaming decodes a large fragmented chunk before the next request"):
      for
        seen <- Ref.make(Chunk.empty[Byte])
        routes = Routes(
          Method.POST / "body" -> Handler(req =>
            req.body.toStream.runCollect
              .mapError(_ => Response.badRequest("body-failed"))
              .flatMap(c => seen.set(c).as(Response.text("body-ok")))
          ),
          Method.GET / "next" -> Handler.text("next-ok"),
        )
        out <- serve(routes, Chunk(post) ++ Chunk.fromIterator(encoded.grouped(97)) ++ Chunk(next))
        got <- seen.get
      yield assertTrue(got.equals(payload), out.contains("body-ok"), out.contains("next-ok"), !out.contains("400"))
    ,
    test("server drains the remainder of a partially consumed wire chunk"):
      val routes = Routes(
        Method.POST / "body" -> Handler(req =>
          req.body.toStream
            .take(1)
            .runCollect
            .mapError(_ => Response.badRequest("body-failed"))
            .map(c => Response.text(s"first-${c.length}"))
        ),
        Method.GET / "next" -> Handler.text("next-ok"),
      )
      serve(routes, Chunk(post) ++ Chunk.fromIterator(encoded.grouped(97)) ++ Chunk(next)).map(out =>
        assertTrue(out.contains("first-1"), out.contains("next-ok"), !out.contains("400"))
      )
    ,
    test("server drains an entirely unread chunked body"):
      val routes =
        Routes(Method.POST / "body" -> Handler.text("ignored"), Method.GET / "next" -> Handler.text("next-ok"))
      serve(routes, Chunk(post) ++ Chunk.fromIterator(encoded.grouped(97)) ++ Chunk(next)).map(out =>
        assertTrue(out.contains("ignored"), out.contains("next-ok"), !out.contains("400"))
      )
    ,
    test("client response streaming and batched reads agree on fragmented wire chunks"):
      val head  = bytes("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
      val input = Chunk(head) ++ Chunk.fromIterator(encoded.grouped(97))
      for
        streamed <- ZIO.scoped {
          connection(input).flatMap { c =>
            Http1Exchange.readHead(c, target, limits, io) *>
              Http1Exchange.streamBody(c, Framing.Chunked, None, limits).toStream.runCollect
          }
        }
        batched <- ZIO.scoped {
          connection(input).flatMap { c =>
            Http1Exchange.readHead(c, target, limits, io) *>
              Http1Exchange.readBody(c, target, Framing.Chunked, limits, io)
          }
        }
      yield assertTrue(streamed == payload, batched == payload, streamed == batched)
    ,
    test("client batched failures preserve protocol error types"):
      ZIO
        .foreach(List("5\r\nabc", "1\r\na\r", "0\r\n")) { raw =>
          ZIO.scoped(
            connection(Chunk(bytes(raw)))
              .flatMap(c => Http1Exchange.readBody(c, target, Framing.Chunked, limits, io).either)
          )
        }
        .map(out =>
          assertTrue(
            out == List(
              Left(ClientError.Protocol(target.authority, HttpError.Malformed(WireError.TruncatedChunk))),
              Left(ClientError.Protocol(target.authority, HttpError.Malformed(WireError.MissingChunkCrlf))),
              Left(ClientError.Protocol(target.authority, HttpError.Malformed(WireError.TruncatedChunk))),
            )
          )
        )
    ,
    test("interrupting a blocked fragment read closes its scoped transport"):
      for
        started <- Promise.make[Nothing, Unit]
        closed  <- Ref.make(false)
        fiber   <- ZIO.scoped {
          connection(Chunk(bytes("5\r\na")), closed, started.succeed(()).unit *> ZIO.never).flatMap { c =>
            Http1Exchange.streamBody(c, Framing.Chunked, None, limits).toStream.runDrain
          }
        }.forkScoped
        _    <- started.await
        exit <- fiber.interrupt
        done <- closed.get
      yield assertTrue(exit.isInterrupted, done),
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.seconds)

  private def bytes(raw: String): Chunk[Byte] = Chunk.fromArray(raw.getBytes(StandardCharsets.US_ASCII))

  private def source(input: Chunk[Chunk[Byte]], capacity: Int): UIO[ConnBuf] =
    Ref.make(input).map { remaining =>
      ConnBuf.fromPull(ByteBuffer.allocate(capacity), remaining.modify(c => (c.headOption, c.drop(1))))
    }

  private def collect(
      src: ConnBuf,
      state: Ref[ConnBuf.ChunkedState],
      maxBody: Long,
      maxLine: Int,
  ): IO[HttpError, Chunk[Chunk[Byte]]] =
    def loop(acc: Chunk[Chunk[Byte]]): IO[HttpError, Chunk[Chunk[Byte]]] =
      src.readChunkedPiece(state, maxBody, maxLine).flatMap {
        case None    => ZIO.succeed(acc)
        case Some(c) => loop(acc :+ c)
      }
    loop(Chunk.empty)

  private def decode(raw: String, maxBody: Long, capacity: Int): IO[HttpError, Chunk[Chunk[Byte]]] =
    for
      src   <- source(Chunk(bytes(raw)), capacity)
      state <- Ref.make(ConnBuf.ChunkedState.initial)
      out   <- collect(src, state, maxBody, capacity)
    yield out

  private def serve(routes: Routes[Any, Response], input: Chunk[Chunk[Byte]]): IO[HttpError, String] =
    ZIO.scoped {
      val config = Server.Config.default.copy(maxHeaderBytes = HeaderBytes(128), chunkSize = ChunkSize(128))
      for
        remaining <- Ref.make(input)
        out       <- Ref.make(Chunk.empty[Byte])
        date      <- OriginDate.make
        _         <- Http1.serveConnection(
          routes,
          remaining.modify(c => (c.headOption, c.drop(1))),
          c => out.update(_ ++ c),
          config,
          Lifecycle(AtomicBoolean(true), AtomicBoolean(false)),
          date,
        )
        result <- out.get
      yield String(result.toArray, StandardCharsets.US_ASCII)
    }

  private def connection(input: Chunk[Chunk[Byte]]): ZIO[Scope, Nothing, Http1Conn] =
    Ref.make(false).flatMap(connection(input, _, ZIO.succeed(-1)))

  private def connection(
      input: Chunk[Chunk[Byte]],
      closed: Ref[Boolean],
      end: IO[HttpError, Int],
  ): ZIO[Scope, Nothing, Http1Conn] =
    for
      remaining <- Ref.make(input)
      conn = new ByteConn:
        def read(dst: ByteBuffer): IO[HttpError, Int] =
          remaining
            .modify { pieces =>
              pieces.headOption match
                case None        => (None, pieces)
                case Some(piece) =>
                  val (out, rest) = piece.splitAt(dst.remaining())
                  val left        = if rest.isEmpty then pieces.drop(1) else rest +: pieces.drop(1)
                  (Some(out), left)
            }
            .flatMap {
              case None      => end
              case Some(out) =>
                ZIO.succeed {
                  dst.put(out.toArray)
                  out.length
                }
            }
        def write(chunk: Chunk[Byte]): IO[HttpError, Unit] = ZIO.unit
        def close: UIO[Unit]                               = closed.set(true)
        def setReadTimeout(d: Duration): UIO[Unit]         = ZIO.unit
      acquired <- ZIO.acquireRelease(ZIO.succeed(conn))(_.close)
    yield Http1Conn(acquired, HeaderBytes(128))
end ChunkedFramingSpec
