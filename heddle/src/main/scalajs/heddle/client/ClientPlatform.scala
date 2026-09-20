package heddle.client

import heddle.http.{Body, Method, Request, Response}
import heddle.http.header.Headers
import heddle.sse.ServerSentEvent
import zio.*
import zio.stream.ZStream

private[heddle] object ClientPlatform:
  def layer: ZLayer[Client.Config, Nothing, Client] =
    ZLayer.succeed(Unsupported)

  def get(url: String, config: Client.Config): Task[Response] =
    fail(s"Client.get($url) $config")

  def request(base: String, req: Request): Task[Response] =
    fail(s"Client.request($base ${req.method})")

  def request(base: String, req: Request, config: Client.Config): Task[Response] =
    fail(s"Client.request($base ${req.method} $config)")

  def request(
      method: Method,
      url: String,
      headers: Headers,
      body: Body,
      config: Client.Config,
  ): Task[Response] =
    fail(s"Client.request($method $url ${headers.toChunk.length} ${body.length} $config)")

  def sse(url: String, config: Client.Config): ZStream[Any, Throwable, ServerSentEvent] =
    ZStream.fail(unsupported(s"Client.sse($url) $config"))

  private object Unsupported extends Client:
    def batched(req: Request): Task[Response] =
      fail(s"Client.batched ${req.method} ${req.url.render}")

  private def fail(what: String): Task[Nothing] =
    ZIO.fail(unsupported(what))

  private def unsupported(what: String): Throwable =
    java.io.IOException(s"$what is not implemented on this platform yet")
end ClientPlatform
