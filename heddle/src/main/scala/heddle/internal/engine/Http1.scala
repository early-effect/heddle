package heddle.internal.engine

import java.nio.charset.StandardCharsets
import heddle.error.HttpError
import heddle.http.{Body, HttpVersion, MediaType, Method, Request, Response, Status, Url}
import heddle.http.header.{Header, HeaderName, Headers}
import heddle.internal.Ascii
import heddle.route.Routes
import heddle.Server
import zio.*
import zio.stream.ZStream

private[heddle] object Http1:
  def serveConnection[R](
      routes: Routes[R, Response],
      pull: IO[HttpError, Option[Chunk[Byte]]],
      send: Chunk[Byte] => Task[Unit],
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
  ): ZIO[R, HttpError, Unit] =
    val buf = java.nio.ByteBuffer.allocate(math.max(config.chunkSize.toInt, config.maxHeaderBytes.toInt))
    serveConnection(routes, ConnBuf.fromPull(buf, pull), send, config, takingWork, busy)
  end serveConnection

  def serveConnection[R](
      routes: Routes[R, Response],
      src: ConnBuf,
      send: Chunk[Byte] => Task[Unit],
      config: Server.Config,
      takingWork: java.util.concurrent.atomic.AtomicBoolean,
      busy: java.util.concurrent.atomic.AtomicBoolean,
      secure: Boolean = false,
  ): ZIO[R, HttpError, Unit] =
    def loop(n: Int): ZIO[R, HttpError, Unit] =
      if !takingWork.get() then ZIO.unit
      else
        nextRequest(src, config, secure, n).foldZIO(
          err =>
            err.toResponse match
              case None      => ZIO.fail(err)
              case Some(res) => writeResponse(send, res, persist = false, version = HttpVersion.Http11)
          ,
          {
            case None                  => ZIO.unit
            case Some((req, leftover)) =>
              busy.set(true)
              dispatch(routes, req).flatMap { res =>
                res.ws match
                  case Some(run) =>
                    writeResponse(send, res, persist = false, req.version) *>
                      leftover.orElse(ZIO.unit) *>
                      run(src, send).mapError(HttpError.Io(_))
                  case None =>
                    val keep =
                      persist(req) && !connectionClose(res) && (n + 1) < config.maxRequestsPerConnection
                    writeResponse(send, res, keep, req.version).flatMap { _ =>
                      leftover.foldZIO(
                        _ =>
                          busy.set(false)
                          ZIO.unit
                        ,
                        _ =>
                          busy.set(false)
                          if keep && takingWork.get() then loop(n + 1) else ZIO.unit,
                      )
                    }
              }
          },
        )
    loop(0)
  end serveConnection

  private def nextRequest(
      src: ConnBuf,
      config: Server.Config,
      secure: Boolean,
      n: Int,
  ): IO[HttpError, Option[(Request, IO[HttpError, Unit])]] =
    if n == 0 then readRequest(src, config, secure)
    else
      waitIdle(src, config.idleTimeout).flatMap {
        case false => ZIO.succeed(None)
        case true  => readRequest(src, config, secure)
      }

  private def waitIdle(src: ConnBuf, idle: Duration): IO[HttpError, Boolean] =
    if idle == Duration.Infinity then src.setReadTimeout(Duration.Infinity) *> src.fillUntil(1).map(_ > 0)
    else if idle.toNanos <= 0L then ZIO.succeed(false)
    else
      src.setReadTimeout(idle) *>
        src
          .fillUntil(1)
          .timeout(idle)
          .map(_.exists(_ > 0))
          .catchSome {
            case HttpError.Io(_: java.nio.channels.ClosedByInterruptException) => ZIO.succeed(false)
            case HttpError.Io(_: java.net.SocketTimeoutException)              => ZIO.succeed(false)
          }

  private def dispatch[R](routes: Routes[R, Response], request: Request): URIO[R, Response] =
    routes(request).catchAllCause { cause =>
      if cause.isInterruptedOnly then ZIO.interrupt
      else ZIO.succeed(Response.internalServerError(cause.prettyPrint))
    }

  private def persist(req: Request): Boolean =
    val conn = req.header(Ascii.Connection)
    if conn.exists(isClose) then false
    else if req.version == HttpVersion.Http10 then conn.exists(isKeepAlive)
    else true

  private def connectionClose(res: Response): Boolean =
    res.header(Ascii.Connection).exists(isClose)

  private def isClose(v: String): Boolean =
    (v eq Ascii.Close) || v.equalsIgnoreCase(Ascii.Close)

  private def isKeepAlive(v: String): Boolean =
    (v eq Ascii.KeepAlive) || v.equalsIgnoreCase(Ascii.KeepAlive)

  private def readRequest(
      src: ConnBuf,
      config: Server.Config,
      secure: Boolean,
  ): IO[HttpError, Option[(Request, IO[HttpError, Unit])]] =
    src.setReadTimeout(config.headerTimeout) *>
      Server
        .awaitWithin(config.headerTimeout)(src.takeHeaders(config.maxHeaderBytes.toInt))
        .catchSome {
          case HttpError.Io(_: java.nio.channels.ClosedByInterruptException) => ZIO.succeed(None)
          case HttpError.Io(_: java.net.SocketTimeoutException)              => ZIO.fail(HttpError.Timeout)
        }
        .flatMap {
          case None      => ZIO.fail(HttpError.Timeout)
          case Some(raw) =>
            raw match
              case None    => ZIO.succeed(None)
              case Some(h) =>
                parseHead(h) match
                  case Left(err)                              => ZIO.fail(err)
                  case Right((method, url, version, headers)) =>
                    requestBody(src, headers, config).map { (body, leftover) =>
                      Some((Request(method, url, headers, body, version, secure), leftover))
                    }
        }

  private def requestBody(
      src: ConnBuf,
      headers: Headers,
      config: Server.Config,
  ): IO[HttpError, (Body, IO[HttpError, Unit])] =
    if chunked(headers) then chunkedBody(src, headers, config)
    else
      headers.get(Ascii.ContentLength) match
        case None      => ZIO.succeed(Body.empty -> ZIO.unit)
        case Some(raw) =>
          raw.toLongOption match
            case None             => ZIO.fail(HttpError.Malformed(s"Invalid Content-Length: $raw"))
            case Some(n) if n < 0 => ZIO.fail(HttpError.Malformed("Negative Content-Length"))
            case Some(n) if n > config.maxBodyBytes.toLong => ZIO.fail(HttpError.BodyTooLarge)
            case Some(0)                                   => ZIO.succeed(Body.empty -> ZIO.unit)
            case Some(n)                                   =>
              Ref.make(n).map { left =>
                val body     = Body.Stream(src.takeBytes(left, config.chunkSize.toInt), headers.contentType, Some(n))
                val leftover = left.get.flatMap(src.drop)
                (body, leftover)
              }

  private def chunked(headers: Headers): Boolean =
    headers.get(Ascii.TransferEncoding).exists { v =>
      (v eq Ascii.Chunked) || v.equalsIgnoreCase(Ascii.Chunked) || v.toLowerCase.contains(Ascii.Chunked)
    }

  private def chunkedBody(
      src: ConnBuf,
      headers: Headers,
      config: Server.Config,
  ): IO[HttpError, (Body, IO[HttpError, Unit])] =
    Ref.make(false).zip(Ref.make(0L)).map { (done, total) =>
      val stream =
        ZStream.repeatZIOChunkOption {
          done.get.flatMap {
            case true  => ZIO.fail(None)
            case false =>
              src
                .readChunkedPiece(total, config.maxBodyBytes.toLong, config.maxHeaderBytes.toInt)
                .mapError(e => Some(src.toThrowable(e)))
                .flatMap {
                  case None    => done.set(true) *> ZIO.fail(None)
                  case Some(c) => ZIO.succeed(c)
                }
          }
        }
      val leftover = done.get.flatMap {
        case true  => ZIO.unit
        case false =>
          def drain: IO[HttpError, Unit] =
            src.readChunkedPiece(total, config.maxBodyBytes.toLong, config.maxHeaderBytes.toInt).flatMap {
              case None    => done.set(true)
              case Some(_) => drain
            }
          drain
      }
      (Body.Stream(stream, headers.contentType, None), leftover)
    }

  private def writeResponse(
      send: Chunk[Byte] => Task[Unit],
      response: Response,
      persist: Boolean,
      version: HttpVersion,
  ): IO[HttpError, Unit] =
    val io: Task[Unit] =
      if persist && version == HttpVersion.Http11 && !response.headers.has(Ascii.Connection) then
        response.body match
          case Body.Empty | Body.Bytes(_, _)   => send(staticHttp11(response))
          case Body.Stream(stream, _, Some(n)) =>
            send(headBytes(response, Some(n), persist, version)) *> sendExactly(send, stream, n)
          case Body.Stream(stream, _, None) =>
            send(headBytes(response, None, persist, version)) *>
              stream.runForeachChunk(chunk => send(chunkedFrame(chunk))) *>
              send(Chunk.fromArray(Ascii.ChunkedEnd))
      else
        response.body match
          case Body.Empty =>
            send(headBytes(response, Some(0L), persist, version))
          case Body.Bytes(bytes, _) =>
            send(headBytes(response, Some(bytes.length.toLong), persist, version)) *> send(bytes)
          case Body.Stream(stream, _, Some(n)) =>
            send(headBytes(response, Some(n), persist, version)) *> sendExactly(send, stream, n)
          case Body.Stream(stream, _, None) =>
            send(headBytes(response, None, persist, version)) *>
              stream.runForeachChunk(chunk => send(chunkedFrame(chunk))) *>
              send(Chunk.fromArray(Ascii.ChunkedEnd))
    io.mapError(HttpError.Io(_))
  end writeResponse

  private[heddle] def staticHttp11(response: Response): Chunk[Byte] =
    val bodyBytes = response.body match
      case Body.Bytes(b, _) => b
      case _                => Chunk.empty[Byte]
    val framed = withContentLength(response.headers, bodyBytes.length.toLong, response.body.mediaType)
    val head   = encodeHead(response.status, framed)
    if bodyBytes.isEmpty then Chunk.fromArray(head)
    else
      val out = Array.ofDim[Byte](head.length + bodyBytes.length)
      java.lang.System.arraycopy(head, 0, out, 0, head.length)
      bodyBytes.copyToArray(out, head.length)
      Chunk.fromArray(out)
  end staticHttp11

  private def sendExactly(
      send: Chunk[Byte] => Task[Unit],
      stream: ZStream[Any, Throwable, Byte],
      n: Long,
  ): Task[Unit] =
    Ref.make(0L).flatMap { written =>
      stream
        .mapChunksZIO { c =>
          written.get.flatMap { w =>
            val rem = n - w
            val out =
              if rem <= 0 then Chunk.empty
              else if c.length.toLong <= rem then c
              else c.take(rem.toInt)
            written.set(w + out.length).as(out)
          }
        }
        .runForeachChunk(send) *>
        written.get.flatMap { w =>
          if w == n then ZIO.unit
          else ZIO.fail(java.io.IOException(s"wrote $w bytes, Content-Length $n"))
        }
    }

  private def headBytes(response: Response, length: Option[Long], persist: Boolean, version: HttpVersion): Chunk[Byte] =
    val framed =
      length match
        case Some(n) => withContentLength(response.headers, n, response.body.mediaType)
        case None    =>
          val h =
            if response.headers.has(HeaderName.TransferEncoding) then response.headers
            else response.headers.add(HeaderName.TransferEncoding, "chunked")
          response.body.mediaType.fold(h) { mt =>
            if h.has(HeaderName.ContentType) then h else h.add(HeaderName.ContentType, mt.render)
          }
    val headers = withConnection(framed, persist, version)
    Chunk.fromArray(encodeHead(response.status, headers))
  end headBytes

  private def encodeHead(status: Status, headers: Headers): Array[Byte] =
    val b                            = Array.newBuilder[Byte]
    def putArr(a: Array[Byte]): Unit =
      var i = 0
      while i < a.length do
        b += a(i)
        i += 1
    def putChunk(c: Chunk[Byte]): Unit =
      c match
        case Chunk.ByteArray(a, o, n) =>
          var i = 0
          while i < n do
            b += a(o + i)
            i += 1
        case _ =>
          var i = 0
          while i < c.length do
            b += c(i)
            i += 1
    def put(s: String): Unit =
      var i = 0
      while i < s.length do
        b += s.charAt(i).toByte
        i += 1
    def crlf(): Unit = putArr(Ascii.Crlf)
    putArr(Ascii.Http11Sp)
    putChunk(status.renderBytes)
    crlf()
    headers.toChunk.foreach { h =>
      put(h.name.render)
      putArr(Ascii.ColonSpace)
      put(h.value)
      crlf()
    }
    crlf()
    b.result()
  end encodeHead

  private def withConnection(headers: Headers, persist: Boolean, version: HttpVersion): Headers =
    if headers.has(Ascii.Connection) then headers
    else if persist && version == HttpVersion.Http10 then headers.add(Ascii.Connection, Ascii.KeepAlive)
    else if persist then headers
    else headers.add(Ascii.Connection, Ascii.Close)

  private def chunkedFrame(chunk: Chunk[Byte]): Chunk[Byte] =
    if chunk.isEmpty then Chunk.empty
    else
      val size = java.lang.Integer.toHexString(chunk.length)
      Chunk.fromArray(size.getBytes(StandardCharsets.US_ASCII)) ++
        Chunk.fromArray(Ascii.Crlf) ++
        chunk ++
        Chunk.fromArray(Ascii.Crlf)

  private def withContentLength(headers: Headers, length: Long, mediaType: Option[MediaType]): Headers =
    val withLen =
      if headers.has(HeaderName.ContentLength) then headers
      else headers.add(HeaderName.ContentLength, length.toString)
    mediaType.fold(withLen) { mt =>
      if withLen.has(HeaderName.ContentType) then withLen else withLen.add(HeaderName.ContentType, mt.render)
    }

  private def parseHead(raw: Array[Byte]): Either[HttpError, (Method, Url, HttpVersion, Headers)] =
    val wrapped = Chunk.fromArray(raw)
    val n       = raw.length
    var i       = 0
    while i + 1 < n && !(raw(i) == '\r' && raw(i + 1) == '\n') do i += 1
    if i + 1 >= n then Left(HttpError.Malformed("Malformed request line"))
    else
      val lineEnd = i
      var sp1     = 0
      while sp1 < lineEnd && raw(sp1) != ' ' do sp1 += 1
      var sp2 = sp1 + 1
      while sp2 < lineEnd && raw(sp2) != ' ' do sp2 += 1
      if sp1 <= 0 || sp2 >= lineEnd then
        Left(HttpError.Malformed(s"Malformed request line: ${Ascii.string(raw, 0, lineEnd)}"))
      else
        Method.parse(wrapped, 0, sp1) match
          case None         => Left(HttpError.Malformed(s"Unknown method: ${Ascii.string(raw, 0, sp1)}"))
          case Some(method) =>
            val url     = Url.parse(wrapped, sp1 + 1, sp2)
            val version = HttpVersion.parse(wrapped, sp2 + 1, lineEnd)
            val hdrs    = scala.collection.mutable.ArrayBuffer.empty[Header]
            i = lineEnd + 2
            var ok             = true
            var err: HttpError = null
            while ok && i + 1 < n do
              if raw(i) == '\r' && raw(i + 1) == '\n' then i = n
              else
                var j = i
                while j + 1 < n && !(raw(j) == '\r' && raw(j + 1) == '\n') do j += 1
                if j + 1 >= n then
                  ok = false
                  err = HttpError.Malformed("Truncated header")
                else
                  var colon = i
                  while colon < j && raw(colon) != ':' do colon += 1
                  if colon <= i || colon >= j then
                    ok = false
                    err = HttpError.Malformed("Malformed header")
                  else
                    val (ns, ne) = Ascii.trim(raw, i, colon)
                    hdrs += Header.slice(HeaderName.intern(raw, ns, ne), raw, colon + 1, j)
                    i = j + 2
                end if
            end while
            if !ok then Left(err)
            else Right((method, url, version, Headers(Chunk.fromIterable(hdrs))))
      end if
    end if
  end parseHead
end Http1
