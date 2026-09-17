package heddle.client

import heddle.error.HttpError
import heddle.http.{Body, Method, Request, Response, Status}
import heddle.http.header.{Header, HeaderName, Headers}
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import heddle.internal.Ascii
import heddle.internal.engine.{ConnBuf, Nio}
import heddle.sse.{ServerSentEvent, SseCodec}
import zio.*
import zio.stream.ZStream

trait Client:
  def batched(req: Request): Task[Response]

object Client:
  def batched(req: Request): ZIO[Client, Throwable, Response] =
    ZIO.serviceWithZIO(_.batched(req))

  final case class Config(
      ssl: javax.net.ssl.SSLContext = javax.net.ssl.SSLContext.getDefault,
      poolSize: Int = 10,
      addUserAgent: Boolean = true,
  )

  def layer: ZLayer[Config, Nothing, Client] =
    ZLayer.scoped {
      for
        cfg  <- ZIO.service[Config]
        idle <- Ref.make(Map.empty[PoolKey, Chunk[Transport]])
        _    <- ZIO.addFinalizer(
          idle.get.flatMap { m =>
            ZIO.foreachDiscard(m.values.flatMap(_.toList))(t => ZIO.succeed(t.close()))
          }
        )
      yield Pooled(cfg, idle)
    }

  val live: ULayer[Client] =
    ZLayer.succeed(Config()) >>> layer

  def get(url: String): Task[Response] =
    request(Method.GET, url, Headers.empty, Body.empty)

  def request(base: String, req: Request): Task[Response] =
    request(req.method, join(base, req.url.render), req.headers, req.body)

  def request(method: Method, url: String, headers: Headers = Headers.empty, body: Body = Body.empty): Task[Response] =
    oneShot(Config(), method, url, headers, body)

  private def oneShot(
      cfg: Config,
      method: Method,
      url: String,
      headers: Headers,
      body: Body,
  ): Task[Response] =
    ZIO.scoped {
      for
        target <- ZIO.attempt(Target.parse(url))
        t      <- ZIO.acquireRelease(ZIO.attempt(open(cfg, target)))(c => ZIO.succeed(c.close()))
        _      <- writeRequest(t, method, target, prepare(cfg, headers), body)
        res    <- readResponse(t)
      yield decodeBody(res)
    }

  private final class Pooled(cfg: Config, idle: Ref[Map[PoolKey, Chunk[Transport]]]) extends Client:
    def batched(req: Request): Task[Response] =
      val url =
        if req.url.absolute then req.url.render
        else
          req.header("Host") match
            case Some(h) =>
              val scheme = if req.secure then "https" else "http"
              s"$scheme://$h${req.url.render}"
            case None => req.url.render
      ZIO.scoped {
        for
          target <- ZIO.attempt(Target.parse(url))
          key = PoolKey(target.scheme, target.host, target.port)
          t   <- acquire(key, target)
          res <- writeRequest(t, req.method, target, prepare(cfg, req.headers), req.body)
            .zipRight(readResponse(t))
            .tapError(_ => ZIO.succeed(t.close()))
          _ <-
            if reusable(req.headers, res.headers) then release(key, t)
            else ZIO.succeed(t.close())
        yield decodeBody(res)
      }
    end batched

    private def acquire(key: PoolKey, target: Target): Task[Transport] =
      idle
        .modify { m =>
          m.getOrElse(key, Chunk.empty) match
            case Chunk() => (None, m)
            case ts      => (Some(ts.head), m.updated(key, ts.drop(1)))
        }
        .flatMap {
          case Some(t) => ZIO.succeed(t)
          case None    => ZIO.attempt(open(cfg, target))
        }

    private def release(key: PoolKey, t: Transport): UIO[Unit] =
      idle.update { m =>
        val cur = m.getOrElse(key, Chunk.empty)
        if cur.length >= cfg.poolSize then
          t.close()
          m
        else m.updated(key, cur :+ t)
      }
  end Pooled

  private def prepare(cfg: Config, headers: Headers): Headers =
    var hdrs = headers
    if cfg.addUserAgent && !hdrs.has(HeaderName.UserAgent) then hdrs = hdrs.add(HeaderName.UserAgent, "heddle")
    if !hdrs.has(HeaderName.AcceptEncoding) then hdrs = hdrs.add(HeaderName.AcceptEncoding, "gzip")
    hdrs

  private def reusable(req: Headers, res: Headers): Boolean =
    !req.get(HeaderName.Connection).exists(_.toLowerCase.contains("close")) &&
      !res.get(HeaderName.Connection).exists(_.toLowerCase.contains("close"))

  private def decodeBody(res: Response): Response =
    if res.headers.contentEncoding.contains(heddle.http.ContentEncoding.Gzip) then
      val raw = res.body.asBytes
      val out = heddle.server.Decompressor.gzip.decompress(raw)
      res
        .copy(headers = res.headers.remove(HeaderName.ContentEncoding).remove(HeaderName.ContentLength))
        .withBody(Body.fromBytes(out, res.body.mediaType))
    else res

  private def open(cfg: Config, target: Target): Transport =
    val ch = SocketChannel.open()
    ch.connect(InetSocketAddress(target.host, target.port))
    if !target.tls then Plain(ch)
    else
      val sock = ch.socket()
      val ssl  = cfg.ssl.getSocketFactory
        .createSocket(sock, target.host, target.port, true)
        .asInstanceOf[javax.net.ssl.SSLSocket]
      ssl.setUseClientMode(true)
      val params = ssl.getSSLParameters
      params.setEndpointIdentificationAlgorithm("HTTPS")
      try params.setServerNames(java.util.List.of(javax.net.ssl.SNIHostName(target.host)))
      catch case _: IllegalArgumentException => ()
      ssl.setSSLParameters(params)
      ssl.startHandshake()
      TlsConn(ssl, ch)
    end if
  end open

  private sealed trait Transport:
    def src: ConnBuf
    def send: Chunk[Byte] => Task[Unit]
    def close(): Unit

  private final case class Plain(ch: SocketChannel) extends Transport:
    def src: ConnBuf                    = ConnBuf.channel(ByteBuffer.allocate(64 * 1024), ch)
    def send: Chunk[Byte] => Task[Unit] =
      val scratch = ByteBuffer.allocate(8192)
      Nio.writer(ch, scratch)
    def close(): Unit = closeQuietly(ch)

  private final case class TlsConn(ssl: javax.net.ssl.SSLSocket, ch: SocketChannel) extends Transport:
    def src: ConnBuf                    = ConnBuf.inputStream(ByteBuffer.allocate(64 * 1024), ssl.getInputStream, ssl)
    def send: Chunk[Byte] => Task[Unit] = heddle.server.Tls.writer(ssl.getOutputStream)
    def close(): Unit                   =
      try ssl.close()
      catch case _: Throwable => ()
      closeQuietly(ch)

  private final case class PoolKey(scheme: String, host: String, port: Int)

  /** Incremental `text/event-stream` parse. Keeps the connection open until the stream ends or is interrupted. */
  def sse(url: String): ZStream[Any, Throwable, ServerSentEvent] =
    ZStream.unwrapScoped {
      for
        target <- ZIO.attempt(Target.parse(url))
        t      <- ZIO.acquireRelease(ZIO.attempt(open(Config(), target)))(c => ZIO.succeed(c.close()))
        _      <- writeRequest(t, Method.GET, target, Headers.empty, Body.empty)
        src = t.src
        raw    <- src.takeHeaders(64 * 1024).mapError(e => java.io.IOException(e.message))
        parsed <- raw match
          case None    => ZIO.fail(java.io.IOException("empty response"))
          case Some(h) => ZIO.fromEither(parseResponse(h)).mapError(java.io.IOException(_))
        (status, headers) = parsed
        _ <- ZIO.fail(java.io.IOException(s"SSE GET $url → $status")).unless(status.code == 200)
      yield decodeSse(bodyStream(src, headers))
    }

  private def closeQuietly(ch: SocketChannel): Unit =
    try ch.close()
    catch case _: Throwable => ()

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

  private def readResponse(t: Transport): Task[Response] =
    val src = t.src
    src
      .takeHeaders(64 * 1024)
      .mapError(e => java.io.IOException(e.message))
      .flatMap {
        case None      => ZIO.fail(java.io.IOException("empty response"))
        case Some(raw) =>
          ZIO.fromEither(parseResponse(raw)).mapError(java.io.IOException(_)).flatMap { (status, headers) =>
            readBody(src, status, headers)
          }
      }
  end readResponse

  private def readBody(src: ConnBuf, status: Status, headers: Headers): Task[Response] =
    val io: IO[HttpError, Response] =
      headers.contentLength match
        case Some(0) => ZIO.succeed(Response(status, headers, Body.empty))
        case Some(n) =>
          src.takeUpTo(n).map { bytes =>
            Response(status, headers, Body.fromBytes(bytes, headers.contentType))
          }
        case None =>
          if headers.get(HeaderName.TransferEncoding).exists(_.toLowerCase.contains("chunked")) then
            Ref.make(0L).flatMap { total =>
              def pieces: IO[HttpError, Chunk[Byte]] =
                src.readChunkedPiece(total, Long.MaxValue, 64 * 1024).flatMap {
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
    val eventStream =
      headers.contentType.exists(_.isEventStream)
    headers.contentLength match
      case Some(n) if !eventStream =>
        ZStream.unwrap(Ref.make(n).map(left => src.takeBytes(left, 8192)))
      case _ if chunked || eventStream =>
        src.chunkedBytes()
      case Some(n) =>
        ZStream.unwrap(Ref.make(n).map(left => src.takeBytes(left, 8192)))
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
end Client
