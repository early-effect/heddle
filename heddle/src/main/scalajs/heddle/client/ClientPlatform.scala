package heddle.client

import heddle.client.internal.ClientSupport
import heddle.error.HttpError
import heddle.http.{Body, Method, Request, Response, Status}
import heddle.http.header.{HeaderName, Headers}
import heddle.internal.node.{AbortController, Buffers, FetchInit, FetchResponse, JsAsync, fetch}
import scala.scalajs.js
import zio.*
import zio.stream.ZStream

/** Node's `fetch`. It pools connections and inflates `Content-Encoding` itself. */
private[heddle] object ClientPlatform:
  def layer: ZLayer[Client.Config, ClientError, Client] =
    ZLayer.fromFunction(FetchClient(_))

  def once(req: Request, config: Client.Config): IO[ClientError, Response] =
    FetchClient(config).batched(req)

  def streaming(req: Request, config: Client.Config): ZIO[Scope, ClientError, Response] =
    FetchClient(config).streaming(req)

  private final class FetchClient(cfg: Client.Config) extends Client:
    def batched(req: Request): IO[ClientError, Response] =
      ZIO.scoped {
        open(req).flatMap { (target, res) =>
          chunks(target, res)
            .runFoldZIO(Chunk.empty[Byte]) { (acc, piece) =>
              if acc.length.toLong + piece.length > cfg.maxBodyBytes.toLong then
                ZIO.fail(ClientError.Protocol(target.authority, HttpError.BodyTooLarge))
              else ZIO.succeed(acc ++ piece)
            }
            .map(bytes => head(res).withBody(Body.fromBytes(bytes, readHeaders(res).contentType)))
        }
      }

    def streaming(req: Request): ZIO[Scope, ClientError, Response] =
      open(req).map { (target, res) =>
        val bytes = chunks(target, res).mapError(e => java.io.IOException(e.message)).flattenChunks
        head(res).withBody(Body.stream(bytes, readHeaders(res).contentType))
      }

    /** The abort controller lives in the caller's scope: closing it cancels the request, even mid-body. */
    private def open(req: Request): ZIO[Scope, ClientError, (Target, FetchResponse)] =
      for
        target <- ZIO.fromEither(Target.of(req))
        raw    <- req.body.collect.mapError(ClientError.Io(target.authority, _))
        ctrl   <- ZIO.acquireRelease(ZIO.succeed(AbortController()))(c => ZIO.succeed(c.abort()))
        sendsBody = raw.nonEmpty && req.method != Method.GET && req.method != Method.HEAD
        init      = FetchInit(
          method = req.method.render,
          headers = toDict(ClientSupport.prepare(cfg, req.headers)),
          body = if sendsBody then Buffers.toU8(raw) else js.undefined,
          redirect = "manual",
          signal = ctrl.signal,
        )
        pending = JsAsync.fromPromise(fetch(target.url, init)).mapError(ClientError.Connect(target.authority, _))
        res <- within(cfg.connectTimeout, ClientError.ConnectTimeout(target.authority))(pending)
      yield (target, res)

    private def chunks(target: Target, res: FetchResponse): ZStream[Any, ClientError, Chunk[Byte]] =
      res.body match
        case null   => ZStream.empty
        case stream =>
          val reader = stream.getReader()
          ZStream.repeatZIOOption {
            val next = JsAsync.fromPromise(reader.read()).mapError(ClientError.Io(target.authority, _))
            within(cfg.idleTimeout, ClientError.ReadTimeout(target.authority))(next).asSomeError.flatMap { r =>
              if r.done then ZIO.fail(None)
              else r.value.toOption.fold(ZIO.fail(None))(u8 => ZIO.succeed(Buffers.fromU8(u8)))
            }
          }

    private def within[A](limit: Duration, late: => ClientError)(io: IO[ClientError, A]): IO[ClientError, A] =
      if limit == Duration.Infinity || limit.toNanos <= 0L then io else io.timeoutFail(late)(limit)

    /** `fetch` already inflated the body, so the encoding headers no longer describe it. */
    private def head(res: FetchResponse): Response =
      val hdrs    = readHeaders(res)
      val decoded =
        if hdrs.has(HeaderName.ContentEncoding) then
          hdrs.remove(HeaderName.ContentEncoding).remove(HeaderName.ContentLength)
        else hdrs
      Response(Status.fromCode(res.status), decoded, Body.empty)

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
      val pairs = scala.collection.mutable.ArrayBuffer.empty[(String, String)]
      res.headers.forEach((value, name) => pairs += (name -> value))
      pairs.foldLeft(Headers.empty)((h, p) => h.add(p._1, p._2))
  end FetchClient
end ClientPlatform
