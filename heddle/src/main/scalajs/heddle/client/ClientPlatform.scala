package heddle.client

import heddle.http.{Body, Method, Request, Response, Status}
import heddle.http.header.{HeaderName, Headers}
import scala.language.implicitConversions
import heddle.internal.node.JsAsync.given
import heddle.internal.node.{AbortController, Buffers, FetchInit, FetchResponse, fetch}
import heddle.sse.{ServerSentEvent, SseCodec}
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import zio.*
import zio.stream.ZStream

private[heddle] object ClientPlatform:
  def layer: ZLayer[Client.Config, Nothing, Client] =
    ZLayer.fromZIO(ZIO.serviceWith[Client.Config](cfg => FetchClient(cfg)))

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
    FetchClient(config).send(method, url, headers, body)

  def sse(url: String, config: Client.Config): ZStream[Any, Throwable, ServerSentEvent] =
    ZStream.unwrapScoped {
      FetchClient(config).sse(url)
    }

  private def join(base: String, path: String): String =
    if path.startsWith("http://") || path.startsWith("https://") then path
    else base.stripSuffix("/") + (if path.startsWith("/") then path else "/" + path)

  private final class FetchClient(cfg: Client.Config) extends Client:
    def batched(req: Request): Task[Response] =
      val url =
        if req.url.absolute then req.url.render
        else
          req.header("Host") match
            case Some(h) =>
              val scheme = if req.secure then "https" else "http"
              s"$scheme://$h${req.url.render}"
            case None => req.url.render
      send(req.method, url, req.headers, req.body)
    end batched

    def send(method: Method, url: String, headers: Headers, body: Body): Task[Response] =
      for
        raw   <- body.collect
        res   <- doFetch(method, url, prepare(headers), raw)
        bytes <- arrayBuffer(res)
        hdrs = readHeaders(res)
        ct   = hdrs.contentType
        out  = Response(Status.fromCode(res.status), hdrs, Body.fromBytes(Buffers.fromU8(bytes), ct))
      yield decodeBody(headers, out)

    def sse(url: String): ZIO[Scope, Throwable, ZStream[Any, Throwable, ServerSentEvent]] =
      doFetch(Method.GET, url, prepare(Headers.empty), Chunk.empty).map { res =>
        if res.status != 200 then ZStream.fail(java.io.IOException(s"SSE GET $url → ${res.status}"))
        else
          bodyStream(res)
            .mapAccum(Chunk.empty[Byte]) { (acc, chunk) =>
              val (events, leftover) = SseCodec.decode(acc ++ chunk)
              (leftover, events)
            }
            .flattenChunks
      }

    private def doFetch(
        method: Method,
        url: String,
        headers: Headers,
        body: Chunk[Byte],
    ): Task[FetchResponse] =
      val ctrl = AbortController()
      val ms   =
        if cfg.connectTimeout == Duration.Infinity || cfg.connectTimeout.toNanos <= 0L then 0L
        else cfg.connectTimeout.toMillis
      val init = FetchInit(
        method = method.render,
        headers = toDict(headers),
        redirect = "manual",
        signal = ctrl.signal,
      )
      if body.nonEmpty && method != Method.GET && method != Method.HEAD then init.body = Buffers.toU8(body)
      val abort =
        if ms <= 0L then ZIO.unit
        else ZIO.sleep(Duration.fromMillis(ms)).as(ctrl.abort()).fork.unit
      abort *> fetch(url, init)
    end doFetch

    private def arrayBuffer(res: FetchResponse): Task[Uint8Array] =
      res.arrayBuffer().map(buf => Uint8Array(buf))

    private def bodyStream(res: FetchResponse): ZStream[Any, Throwable, Chunk[Byte]] =
      res.body match
        case null   => ZStream.empty
        case stream =>
          val reader = stream.getReader()
          ZStream.repeatZIOOption {
            val pulled: Task[heddle.internal.node.StreamReadResult] = reader.read()
            pulled.mapError(Some(_)).flatMap { chunk =>
              if chunk.done then ZIO.fail(None)
              else
                chunk.value.toOption match
                  case Some(u8) => ZIO.succeed(Buffers.fromU8(u8))
                  case None     => ZIO.fail(None)
            }
          }

    private def prepare(headers: Headers): Headers =
      var hdrs = headers
      if cfg.addUserAgent && !hdrs.has(HeaderName.UserAgent) then hdrs = hdrs.add(HeaderName.UserAgent, "heddle")
      hdrs

    private def toDict(headers: Headers): js.Dictionary[String] =
      val d = js.Dictionary.empty[String]
      headers.toChunk.foreach { h =>
        val name = h.name.render
        if !forbidden(name) then d.update(name, h.value)
      }
      d

    private def forbidden(name: String): Boolean =
      val n = name.toLowerCase
      n == "host" || n == "connection" || n == "content-length" || n == "transfer-encoding" || n == "keep-alive"

    private def readHeaders(res: FetchResponse): Headers =
      var hdrs = Headers.empty
      res.headers.forEach { (value, name) =>
        hdrs = hdrs.add(name, value)
      }
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
  end FetchClient
end ClientPlatform
