package heddle.sse

import heddle.http.{Body, MediaType, Response, Status}
import heddle.http.header.HeaderName
import zio.*
import zio.stream.ZStream

object Sse:
  trait Writer:
    def send(event: ServerSentEvent): UIO[Unit]
    def heartbeat: UIO[Unit]

  def body(events: ZStream[Any, Throwable, ServerSentEvent]): Body =
    Body.stream(
      events.flatMap(e => ZStream.fromChunk(SseCodec.encode(e))),
      contentType = Some(MediaType.EventStream),
      length = None,
    )

  def response(events: ZStream[Any, Throwable, ServerSentEvent]): Response =
    Response(Status.Ok)
      .withHeader(HeaderName.CacheControl, "no-cache")
      .withBody(body(events))

  def heartbeatEvery(d: Duration): ZStream[Any, Nothing, ServerSentEvent] =
    ZStream.tick(d).as(ServerSentEvent.Heartbeat)

  /** Queue-backed session: `use` writes events; the response body is that stream. The writer fiber has `Scope`. */
  def session[R](use: Writer => ZIO[R & Scope, Throwable, Unit]): URIO[R, Response] =
    ZIO.environmentWith[R] { env =>
      val events =
        ZStream.unwrapScoped {
          for
            q <- Queue.unbounded[Option[ServerSentEvent]]
            writer = new Writer:
              def send(event: ServerSentEvent): UIO[Unit] = q.offer(Some(event)).unit
              def heartbeat: UIO[Unit]                    = q.offer(Some(ServerSentEvent.Heartbeat)).unit
            _ <- use(writer)
              .provideSomeEnvironment[Scope](scopeEnv => env.unionAll(scopeEnv))
              .ensuring(q.offer(None).unit)
              .forkScoped
          yield ZStream.fromQueue(q).takeWhile(_.isDefined).collect { case Some(e) => e }
        }
      response(events)
    }
end Sse
