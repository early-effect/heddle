package heddle.client.internal

import heddle.client.{Client, ClientError, Target}
import heddle.http.{Body, ContentEncoding, Response}
import heddle.http.header.{HeaderName, Headers}
import heddle.server.Compressor
import zio.*

/** Request and response handling every platform client shares. */
private[heddle] object ClientSupport:
  def prepare(cfg: Client.Config, headers: Headers): Headers =
    if cfg.addUserAgent && !headers.has(HeaderName.UserAgent) then headers.add(HeaderName.UserAgent, "heddle")
    else headers

  /** Gunzips the body when the caller asked for gzip and the server sent it, within `maxBodyBytes`. */
  def inflate(cfg: Client.Config, target: Target, requested: Headers)(res: Response): IO[ClientError, Response] =
    val asked = requested.get(HeaderName.AcceptEncoding).exists(_.toLowerCase.contains("gzip"))
    if !asked || res.headers.contentEncoding != Chunk(ContentEncoding.Gzip) then ZIO.succeed(res)
    else
      res.body match
        case Body.Bytes(raw, mediaType) =>
          ZIO
            .fromEither(Compressor.gunzip(raw, cfg.maxBodyBytes))
            .mapBoth(
              ClientError.Protocol(target.authority, _),
              out =>
                res
                  .copy(headers = res.headers.remove(HeaderName.ContentEncoding).remove(HeaderName.ContentLength))
                  .withBody(Body.fromBytes(out, mediaType)),
            )
        case _ => ZIO.succeed(res)
    end if
  end inflate
end ClientSupport
