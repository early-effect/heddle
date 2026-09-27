package heddle.client.internal

import heddle.client.{Authority, Client, ClientError, Target}
import heddle.error.{HttpError, WireError}
import heddle.http.{Body, HttpVersion, Method, Response, Status, TransferCoding}
import heddle.http.header.{Header, HeaderName, Headers}
import heddle.internal.Ascii
import heddle.internal.duplex.ByteConn
import heddle.internal.engine.ConnBuf
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import zio.*
import zio.stream.ZStream

/** One open HTTP/1.1 connection. The read buffer lives as long as the connection, so bytes read ahead survive reuse. */
private[heddle] final class Http1Conn(val conn: ByteConn):
  val src: ConnBuf = ConnBuf.fromConn(ByteBuffer.allocate(64 * 1024), conn)

/** Whether a connection may carry another exchange once this one is fully read. */
private[heddle] enum Reuse:
  case Keep, Close

/** How a response body ends (RFC 9112 §6.3). */
private[heddle] enum Framing:
  case NoBody
  case Length(bytes: Long)
  case Chunked
  case UntilClose

/** Maps a transport failure onto `ClientError`. Platforms know which exceptions mean "timed out". */
private[heddle] trait IoFailure:
  def apply(authority: Authority, cause: Throwable): ClientError

/** Response read limits from `Client.Config`. `idle` bounds every read at the fiber level, which is the only timeout a
  * non-blocking socket honors. `maxHead` caps the header block and each chunk-size line.
  */
private[heddle] final case class Limits(idle: Duration, maxHead: Int, maxBody: Long):
  def within[A](a: Authority)(read: IO[ClientError, A]): IO[ClientError, A] =
    if idle == Duration.Infinity || idle.toNanos <= 0L then read
    else read.timeoutFail(ClientError.ReadTimeout(a))(idle)

private[heddle] object Limits:
  def of(cfg: Client.Config): Limits =
    Limits(cfg.idleTimeout, cfg.maxHeaderBytes.toInt, cfg.maxBodyBytes.toLong)

private[heddle] object Http1Exchange:
  final case class Head(version: HttpVersion, status: Status, headers: Headers)

  def send(
      c: Http1Conn,
      target: Target,
      method: Method,
      headers: Headers,
      body: Body,
      io: IoFailure,
  ): IO[ClientError, Unit] =
    val withHost = if headers.has(HeaderName.Host) then headers else headers.add(HeaderName.Host, target.hostHeader)
    val withLen  = body.length match
      case Some(n) if !withHost.has(HeaderName.ContentLength) => withHost.add(HeaderName.ContentLength, n.toString)
      case _                                                  => withHost
    val complete = body.mediaType match
      case Some(mt) if !withLen.has(HeaderName.ContentType) => withLen.add(HeaderName.ContentType, mt.render)
      case _                                                => withLen
    val head = Chunk.fromArray(
      (s"${method.render} ${target.requestTarget} HTTP/1.1\r\n" +
        complete.toChunk.map(h => s"${h.name.render}: ${h.value}\r\n").mkString + "\r\n")
        .getBytes(StandardCharsets.US_ASCII)
    )
    val wrote = body match
      case Body.Empty           => c.conn.write(head)
      case Body.Bytes(bytes, _) => c.conn.write(head ++ bytes)
      case Body.Stream(s, _, _) => c.conn.write(head) *> s.runForeachChunk(c.conn.write)
    wrote.mapError(io(target.authority, _))
  end send

  def readHead(c: Http1Conn, target: Target, limits: Limits, io: IoFailure): IO[ClientError, Head] =
    limits
      .within(target.authority)(c.src.takeHeaders(limits.maxHead).mapError(fromHttp(target.authority, io)))
      .flatMap {
        case None      => ZIO.fail(io(target.authority, java.io.EOFException("connection closed before a response")))
        case Some(raw) =>
          ZIO.fromEither(parseHead(raw)).mapError(malformed(target))
      }

  def malformed(target: Target)(e: WireError): ClientError =
    ClientError.Protocol(target.authority, HttpError.Malformed(e))

  /** RFC 9112 §6.3: a Content-Length that is not `1*DIGIT` leaves the response unframed, so it fails. */
  def framing(method: Method, head: Head): Either[WireError, Framing] =
    val code = head.status.code
    if method == Method.HEAD || code / 100 == 1 || code == 204 || code == 304 then Right(Framing.NoBody)
    else if head.headers.transferEncoding.lastOption.contains(TransferCoding.Chunked) then Right(Framing.Chunked)
    // RFC 9112 §6.3: a response whose last coding is not chunked runs until the connection closes.
    else if head.headers.transferEncoding.nonEmpty then Right(Framing.UntilClose)
    else
      head.headers.get(HeaderName.ContentLength) match
        case None      => Right(Framing.UntilClose)
        case Some(raw) => Ascii.decimal(raw).map(Framing.Length(_)).toRight(WireError.BadContentLength(raw))
  end framing

  def reuse(request: Headers, head: Head, framing: Framing): Reuse =
    val closes    = (h: Headers) => h.connection.contains("close")
    val keepAlive = head.headers.connection.contains("keep-alive")
    val persists  = head.version match
      case HttpVersion.Http11 => true
      case _                  => keepAlive
    if framing == Framing.UntilClose || closes(request) || closes(head.headers) || !persists then Reuse.Close
    else Reuse.Keep

  /** Reads the whole body, never more than `limits.maxBody` bytes. Each read may idle for `limits.idle`. */
  def readBody(
      c: Http1Conn,
      target: Target,
      framing: Framing,
      limits: Limits,
      io: IoFailure,
  ): IO[ClientError, Chunk[Byte]] =
    val a                                              = target.authority
    val tooLarge                                       = ClientError.Protocol(a, HttpError.BodyTooLarge)
    def piece(max: Long): IO[ClientError, Chunk[Byte]] =
      limits.within(a)(c.src.takeUpTo(max.min(8192L)).mapError(fromHttp(a, io)))
    framing match
      case Framing.NoBody                          => ZIO.succeed(Chunk.empty)
      case Framing.Length(n) if n > limits.maxBody => ZIO.fail(tooLarge)
      case Framing.Length(n)                       =>
        def rest(acc: Chunk[Byte]): IO[ClientError, Chunk[Byte]] =
          if acc.length.toLong >= n then ZIO.succeed(acc)
          else
            piece(n - acc.length).flatMap { got =>
              if got.isEmpty then ZIO.fail(io(a, java.io.EOFException(s"body ended at ${acc.length} of $n bytes")))
              else rest(acc ++ got)
            }
        rest(Chunk.empty)
      case Framing.Chunked =>
        Ref.make(0L).flatMap { total =>
          def pieces(acc: Chunk[Byte]): IO[ClientError, Chunk[Byte]] =
            limits
              .within(a)(c.src.readChunkedPiece(total, limits.maxBody, limits.maxHead).mapError(fromHttp(a, io)))
              .flatMap {
                case None      => ZIO.succeed(acc)
                case Some(got) => pieces(acc ++ got)
              }
          pieces(Chunk.empty)
        }
      case Framing.UntilClose =>
        def rest(acc: Chunk[Byte]): IO[ClientError, Chunk[Byte]] =
          piece(8192L).flatMap { got =>
            if got.isEmpty then ZIO.succeed(acc)
            else if acc.length.toLong + got.length > limits.maxBody then ZIO.fail(tooLarge)
            else rest(acc ++ got)
          }
        rest(Chunk.empty)
    end match
  end readBody

  /** The body as it arrives. The connection stays open until the stream ends or its scope closes. */
  def streamBody(c: Http1Conn, framing: Framing, contentType: Option[heddle.http.MediaType]): Body =
    val bytes: ZStream[Any, Throwable, Byte] = framing match
      case Framing.NoBody     => ZStream.empty
      case Framing.Length(n)  => ZStream.unwrap(Ref.make(n).map(left => c.src.takeBytes(left, 8192)))
      case Framing.Chunked    => c.src.chunkedBytes()
      case Framing.UntilClose =>
        ZStream.repeatZIOChunkOption {
          c.src.takeUpTo(8192).mapError(e => Some(c.src.toThrowable(e))).flatMap { piece =>
            if piece.isEmpty then ZIO.fail(None) else ZIO.succeed(piece)
          }
        }
    Body.stream(
      bytes,
      contentType,
      framing match
        case Framing.Length(n) => Some(n)
        case _                 => None,
    )
  end streamBody

  def response(head: Head, body: Body): Response =
    Response(head.status, head.headers, body)

  private def fromHttp(authority: Authority, io: IoFailure)(e: HttpError): ClientError =
    e match
      case HttpError.Io(cause) => io(authority, cause)
      case HttpError.Timeout   => ClientError.ReadTimeout(authority)
      case other               => ClientError.Protocol(authority, other)

  private[heddle] def parseHead(raw: Array[Byte]): Either[WireError, Head] =
    val n = raw.length
    var i = 0
    while i + 1 < n && !(raw(i) == '\r' && raw(i + 1) == '\n') do i += 1
    if i + 1 >= n then Left(WireError.BadStatusLine(Ascii.string(raw, 0, n)))
    else
      val line = Ascii.string(raw, 0, i)
      val sp1  = line.indexOf(' ')
      val sp2  = if sp1 < 0 then -1 else line.indexOf(' ', sp1 + 1)
      if sp1 < 0 then Left(WireError.BadStatusLine(line))
      else
        val version = HttpVersion.parse(Chunk.fromArray(raw), 0, sp1)
        val codeStr = if sp2 < 0 then line.substring(sp1 + 1) else line.substring(sp1 + 1, sp2)
        codeStr.toIntOption.filter(c => c >= 100 && c <= 999) match
          case None    => Left(WireError.BadStatusLine(line))
          case Some(c) =>
            val hdrs = scala.collection.mutable.ArrayBuffer.empty[Header]
            var j    = i + 2
            var err  = Option.empty[WireError]
            while err.isEmpty && j + 1 < n do
              if raw(j) == '\r' && raw(j + 1) == '\n' then j = n
              else
                var k = j
                while k + 1 < n && !(raw(k) == '\r' && raw(k + 1) == '\n') do k += 1
                if k + 1 >= n then err = Some(WireError.TruncatedHeaders)
                else
                  var colon = j
                  while colon < k && raw(colon) != ':' do colon += 1
                  if colon <= j || colon >= k then err = Some(WireError.BadHeaderLine)
                  else
                    val (ns, ne) = Ascii.trim(raw, j, colon)
                    hdrs += Header.slice(HeaderName.intern(raw, ns, ne), raw, colon + 1, k)
                    j = k + 2
                end if
            end while
            err.toLeft(Head(version, Status.fromCode(c), Headers(Chunk.fromIterable(hdrs))))
        end match
      end if
    end if
  end parseHead
end Http1Exchange
