package heddle.client

import heddle.error.HttpError
import heddle.http.{Body, Method, Request, Response, Status}
import heddle.http.header.{Header, HeaderName, Headers}
import heddle.internal.Ascii
import heddle.internal.duplex.{ByteConn, NativeConn}
import heddle.internal.engine.ConnBuf
import heddle.internal.openssl.Ssl
import heddle.internal.posix.Net
import heddle.sse.{ServerSentEvent, SseCodec}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import zio.*
import zio.stream.ZStream

private[heddle] object ClientPlatform:
  def layer: ZLayer[Client.Config, Nothing, Client] =
    ZLayer.fromZIO(ZIO.serviceWith[Client.Config](cfg => Live(cfg)))

  def get(url: String, config: Client.Config): Task[Response] =
    request(Method.GET, url, Headers.empty, Body.empty, config)

  def request(base: String, req: Request): Task[Response] =
    request(req.method, join(base, req.url.render), req.headers, req.body, Client.Config.default)

  def request(base: String, req: Request, config: Client.Config): Task[Response] =
    request(req.method, join(base, req.url.render), req.headers, req.body, config)

  def request(
      method: Method,
      url: String,
      headers: Headers,
      body: Body,
      config: Client.Config,
  ): Task[Response] =
    oneShot(config, method, url, headers, body)

  def sse(url: String, config: Client.Config): ZStream[Any, Throwable, ServerSentEvent] =
    ZStream.unwrapScoped {
      for
        target <- ZIO.attempt(Target.parse(url))
        conn   <- ZIO.acquireRelease(open(config, target))(_.close)
        t = Transport(conn)
        _ <- writeRequest(t, Method.GET, target, prepare(config, Headers.empty), Body.empty)
        src = t.src
        _      <- src.setReadTimeout(config.idleTimeout)
        raw    <- src.takeHeaders(config.maxHeaderBytes.toInt).mapError(e => java.io.IOException(e.message))
        parsed <- raw match
          case None    => ZIO.fail(java.io.IOException("empty response"))
          case Some(h) => ZIO.fromEither(parseResponse(h)).mapError(java.io.IOException(_))
        (status, headers) = parsed
        _ <- ZIO.fail(java.io.IOException(s"SSE GET $url → $status")).unless(status.code == 200)
      yield decodeSse(bodyStream(src, headers))
    }

  private final class Live(cfg: Client.Config) extends Client:
    def batched(req: Request): Task[Response] =
      val url =
        if req.url.absolute then req.url.render
        else
          req.header("Host") match
            case Some(h) =>
              val scheme = if req.secure then "https" else "http"
              s"$scheme://$h${req.url.render}"
            case None => req.url.render
      oneShot(cfg, req.method, url, req.headers, req.body)
    end batched
  end Live

  private def oneShot(
      cfg: Client.Config,
      method: Method,
      url: String,
      headers: Headers,
      body: Body,
  ): Task[Response] =
    val prepared = prepare(cfg, headers)
    ZIO.scoped {
      for
        target <- ZIO.attempt(Target.parse(url))
        conn   <- ZIO.acquireRelease(open(cfg, target))(_.close)
        t = Transport(conn)
        _   <- writeRequest(t, method, target, prepared, body)
        res <- readResponse(cfg, t)
      yield decodeBody(prepared, res)
    }
  end oneShot

  private def open(cfg: Client.Config, target: Target): Task[ByteConn] =
    val _ = cfg
    ZIO.attemptBlockingInterrupt {
      val fd = Net.connect(target.host, target.port)
      val ms =
        if cfg.connectTimeout == Duration.Infinity || cfg.connectTimeout.toNanos <= 0L then 0
        else math.max(1L, cfg.connectTimeout.toMillis).min(Int.MaxValue.toLong).toInt
      if ms > 0 then Net.setRecvTimeout(fd, ms)
      if !target.tls then NativeConn.of(fd)
      else
        val ctx = Ssl.clientCtx()
        try
          val sni = if target.host.exists(c => c >= 'A' && c <= 'z') then target.host else "localhost"
          NativeConn.tls(fd, Ssl.connect(ctx, fd, sni))
        catch
          case e: Throwable =>
            ctx.close()
            Net.close(fd)
            throw e
      end if
    }
  end open

  private final class Transport(conn: ByteConn):
    def src: ConnBuf                    = ConnBuf.fromConn(ByteBuffer.allocate(64 * 1024), conn)
    def send: Chunk[Byte] => Task[Unit] = conn.write

  private def prepare(cfg: Client.Config, headers: Headers): Headers =
    var hdrs = headers
    if cfg.addUserAgent && !hdrs.has(HeaderName.UserAgent) then hdrs = hdrs.add(HeaderName.UserAgent, "heddle")
    hdrs

  private def decodeBody(reqHeaders: Headers, res: Response): Response =
    val asked = reqHeaders.get(HeaderName.AcceptEncoding).exists(_.toLowerCase.contains("gzip"))
    if asked && res.headers.contentEncoding.contains(heddle.http.ContentEncoding.Gzip) then
      val raw = res.body.asBytes
      val out = heddle.server.Decompressor.gzip.decompress(raw)
      res
        .copy(headers = res.headers.remove(HeaderName.ContentEncoding).remove(HeaderName.ContentLength))
        .withBody(Body.fromBytes(out, res.body.mediaType))
    else res

  private def writeRequest(
      t: Transport,
      method: Method,
      target: Target,
      headers: Headers,
      body: Body,
  ): Task[Unit] =
    val send = t.send
    val len  = body.length
    var hdrs = headers
    if !hdrs.has(HeaderName.Host) then hdrs = hdrs.add(HeaderName.Host, target.hostHeader)
    len.foreach(n => if !hdrs.has(HeaderName.ContentLength) then hdrs = hdrs.add(HeaderName.ContentLength, n.toString))
    body.mediaType.foreach(mt =>
      if !hdrs.has(HeaderName.ContentType) then hdrs = hdrs.add(HeaderName.ContentType, mt.render)
    )
    val head =
      s"${method.render} ${target.path} HTTP/1.1\r\n" +
        hdrs.toChunk.map(h => s"${h.name.render}: ${h.value}\r\n").mkString + "\r\n"
    send(Chunk.fromArray(head.getBytes(StandardCharsets.US_ASCII))) *>
      (body match
        case Body.Empty           => ZIO.unit
        case Body.Bytes(bytes, _) => send(bytes)
        case Body.Stream(s, _, _) => s.runForeachChunk(send))
  end writeRequest

  private def readResponse(cfg: Client.Config, t: Transport): Task[Response] =
    val src = t.src
    src.setReadTimeout(cfg.idleTimeout) *>
      src
        .takeHeaders(cfg.maxHeaderBytes.toInt)
        .mapError(e => java.io.IOException(e.message))
        .flatMap {
          case None      => ZIO.fail(java.io.IOException("empty response"))
          case Some(raw) =>
            ZIO.fromEither(parseResponse(raw)).mapError(java.io.IOException(_)).flatMap { (status, headers) =>
              readBody(cfg, src, status, headers)
            }
        }
  end readResponse

  private def readBody(cfg: Client.Config, src: ConnBuf, status: Status, headers: Headers): Task[Response] =
    val io: IO[HttpError, Response] =
      headers.contentLength match
        case Some(0) => ZIO.succeed(Response(status, headers, Body.empty))
        case Some(n) =>
          if n > cfg.maxBodyBytes.toLong then ZIO.fail(HttpError.BodyTooLarge)
          else
            src.takeUpTo(n).map { bytes =>
              Response(status, headers, Body.fromBytes(bytes, headers.contentType))
            }
        case None =>
          if headers.get(HeaderName.TransferEncoding).exists(_.toLowerCase.contains("chunked")) then
            Ref.make(0L).flatMap { total =>
              def pieces: IO[HttpError, Chunk[Byte]] =
                src.readChunkedPiece(total, cfg.maxBodyBytes.toLong, cfg.maxHeaderBytes.toInt).flatMap {
                  case None    => ZIO.succeed(Chunk.empty)
                  case Some(c) => pieces.map(c ++ _)
                }
              pieces.map(b => Response(status, headers, Body.fromBytes(b, headers.contentType)))
            }
          else
            def rest(acc: Chunk[Byte]): IO[HttpError, Chunk[Byte]] =
              src.takeUpTo(8192).flatMap { c =>
                if c.isEmpty then ZIO.succeed(acc) else rest(acc ++ c)
              }
            rest(Chunk.empty).map(b => Response(status, headers, Body.fromBytes(b, headers.contentType)))
    io.mapError(e => java.io.IOException(e.message))
  end readBody

  private def bodyStream(src: ConnBuf, headers: Headers): ZStream[Any, Throwable, Byte] =
    val chunked =
      headers.get(HeaderName.TransferEncoding).exists(_.toLowerCase.contains("chunked"))
    headers.contentLength match
      case Some(n) =>
        ZStream.unwrap(Ref.make(n).map(left => src.takeBytes(left, 8192)))
      case _ if chunked =>
        src.chunkedBytes()
      case None =>
        ZStream.repeatZIOChunkOption {
          src.takeUpTo(8192).mapError(e => Some(src.toThrowable(e))).flatMap { c =>
            if c.isEmpty then ZIO.fail(None) else ZIO.succeed(c)
          }
        }
    end match
  end bodyStream

  private def decodeSse(bytes: ZStream[Any, Throwable, Byte]): ZStream[Any, Throwable, ServerSentEvent] =
    bytes.chunks
      .mapAccum(Chunk.empty[Byte]) { (acc, chunk) =>
        val (evs, rest) = SseCodec.decode(acc ++ chunk)
        (rest, evs)
      }
      .flattenChunks

  private def parseResponse(raw: Array[Byte]): Either[String, (Status, Headers)] =
    val n = raw.length
    var i = 0
    while i + 1 < n && !(raw(i) == '\r' && raw(i + 1) == '\n') do i += 1
    if i + 1 >= n then Left("Malformed status line")
    else
      val line = Ascii.string(raw, 0, i)
      val sp1  = line.indexOf(' ')
      val sp2  = if sp1 < 0 then -1 else line.indexOf(' ', sp1 + 1)
      if sp1 < 0 then Left(s"Malformed status line: $line")
      else
        val codeStr = if sp2 < 0 then line.substring(sp1 + 1) else line.substring(sp1 + 1, sp2)
        codeStr.toIntOption match
          case None    => Left(s"Malformed status line: $line")
          case Some(c) =>
            val hdrs = scala.collection.mutable.ArrayBuffer.empty[Header]
            var j    = i + 2
            var ok   = true
            var err  = ""
            while ok && j + 1 < n do
              if raw(j) == '\r' && raw(j + 1) == '\n' then j = n
              else
                var k = j
                while k + 1 < n && !(raw(k) == '\r' && raw(k + 1) == '\n') do k += 1
                if k + 1 >= n then
                  ok = false
                  err = "Truncated header"
                else
                  var colon = j
                  while colon < k && raw(colon) != ':' do colon += 1
                  if colon <= j || colon >= k then
                    ok = false
                    err = "Malformed header"
                  else
                    val (ns, ne) = Ascii.trim(raw, j, colon)
                    hdrs += Header.slice(HeaderName.intern(raw, ns, ne), raw, colon + 1, k)
                    j = k + 2
                end if
            end while
            if !ok then Left(err) else Right((Status.fromCode(c), Headers(Chunk.fromIterable(hdrs))))
        end match
      end if
    end if
  end parseResponse

  private def join(base: String, path: String): String =
    val b = base.stripSuffix("/")
    if path.startsWith("http://") || path.startsWith("https://") then path
    else if path.startsWith("/") then b + path
    else b + "/" + path

  private final case class Target(scheme: String, host: String, port: Int, path: String, hostHeader: String):
    def tls: Boolean = scheme == "https"

  private object Target:
    def parse(url: String): Target =
      val u      = java.net.URI(url)
      val scheme = Option(u.getScheme).getOrElse("http").toLowerCase
      val host   = Option(u.getHost).getOrElse("127.0.0.1")
      val port   =
        if u.getPort > 0 then u.getPort
        else if scheme == "https" then 443
        else 80
      val p    = Option(u.getRawPath).filter(_.nonEmpty).getOrElse("/")
      val path = Option(u.getRawQuery).fold(p)(q => s"$p?$q")
      val hh   = if u.getPort > 0 then s"$host:${u.getPort}" else host
      Target(scheme, host, port, path, hh)
    end parse
  end Target
end ClientPlatform
