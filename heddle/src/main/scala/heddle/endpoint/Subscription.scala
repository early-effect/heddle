package heddle.endpoint

import heddle.client.{CallFailure, ClientError}
import heddle.http.{Request, Response, Status, Url}
import heddle.route.{Outcome, Routes}
import heddle.sse.{ServerSentEvent, Sse, SseCodec, SseField}
import zio.json.JsonCodec
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Duration, IO, ZIO}

/** One fact on a subscription. `id` is the application's resume id. The next call passes it back through the
  * operation's ordinary input. Heddle does not invent it and does not store it.
  */
final case class Observed[A](id: SseField, value: A)

/** A streamed operation. The success body is `text/event-stream`. `Client.call` does not accept it.
  *
  * `open` is the gate: its failure is the operation's error and no stream is sent. The `ZStream` it returns is one
  * listener. Two requests are two pulls. Interrupting a pull runs that stream's finalizers only.
  */
final class Subscription[In, Err, A] private[endpoint] (
    val endpoint: Endpoint[In, Err, ?],
    val doc: EndpointDoc,
    val element: SchemaDoc,
    codec: JsonCodec[A],
    serve: [R] => (In => ZIO[R, Err, ZStream[R, Err, Observed[A]]]) => Routes[R, Response],
):
  def toRequest(in: In, base: Url): Request = endpoint.toRequest(in, base)

  /** `A` as JSON. A codec that cannot produce JSON becomes `null`. */
  private[heddle] def dataJson(value: A): Json =
    codec.encoder.toJsonAST(value).getOrElse(Json.Null)

  def bind[R](
      open: In => ZIO[R, Err, ZStream[R, Err, Observed[A]]]
  ): BoundSubscription[R, In, Err, A] =
    BoundSubscription(this, open, serve(open))

  /** A gate failure is `Domain` and no elements. A success body is the event stream. */
  def read(res: Response): IO[CallFailure[Err], ZStream[Any, CallFailure[Err], Observed[A]]] =
    if endpoint.errors.handles(res.status) then
      endpoint.errors
        .decode(res.status, res.body)
        .mapError(err => CallFailure.Undecodable(res.status, OutputError.Body(err)))
        .flatMap {
          case Some(error) => ZIO.fail(CallFailure.Domain(error))
          case None        => ZIO.fail(CallFailure.Unexpected(res.status))
        }
    else if !res.status.isSuccess then ZIO.fail(CallFailure.Unexpected(res.status))
    else ZIO.succeed(elements(res.body.toStream))

  private def elements(bytes: ZStream[Any, Throwable, Byte]): ZStream[Any, CallFailure[Err], Observed[A]] =
    SseCodec
      .stream(bytes)
      .mapError(cause => CallFailure.Transport(ClientError.BodyFailed(cause)))
      .mapZIO { event =>
        event.event.map(_.value) match
          case Some(Subscription.ErrorEvent) => failure(event.data)
          case _                             => value(event)
      }

  private def value(event: ServerSentEvent): IO[CallFailure[Err], Observed[A]] =
    event.id match
      case None     => ZIO.fail(undecodable("missing id"))
      case Some(id) =>
        ZIO
          .fromEither(codec.decoder.decodeJson(event.data))
          .mapBoth(detail => undecodable(detail), Observed(id, _))

  /** A terminal error ends the stream. It is not an element. */
  private def failure(data: String): IO[CallFailure[Err], Nothing] =
    endpoint.errors
      .decodeDocument(data)
      .mapError(err => CallFailure.Undecodable(Status.Ok, OutputError.Body(err)))
      .flatMap {
        case Some(error) => ZIO.fail(CallFailure.Domain(error))
        case None        => ZIO.fail(undecodable(data))
      }

  private def undecodable(detail: String): CallFailure[Err] =
    CallFailure.Undecodable(Status.Ok, OutputError.Body(BodyError.Json(detail)))
end Subscription

object Subscription:
  /** SSE `event` name of the one terminal error a subscription sends after it has opened. */
  val ErrorEvent: String = "error"

  /** Comment heartbeats on an open HTTP subscription. The application does not send these. */
  val Heartbeat: Duration = Duration.fromSeconds(15)

  private[endpoint] def respond[R, Err, A](
      errors: ErrorCodec[Err],
      codec: JsonCodec[A],
      gate: ZIO[R, Err, ZStream[R, Err, Observed[A]]],
  ): ZIO[R, Nothing, Response] =
    for
      env    <- ZIO.environment[R]
      opened <- gate.either
    yield opened match
      case Left(error)   => errors.encode(error)
      case Right(stream) =>
        val facts = stream.provideEnvironment(env).map(item => fact(codec, item)).catchAll { error =>
          ZStream.succeed(terminal(errors, error))
        }
        Sse.response(facts.mergeHaltLeft(Sse.heartbeatEvery(Heartbeat)))

  private def fact[A](codec: JsonCodec[A], item: Observed[A]): ServerSentEvent =
    ServerSentEvent(codec.encoder.encodeJson(item.value).toString, id = Some(item.id))

  private def terminal[Err](errors: ErrorCodec[Err], error: Err): ServerSentEvent =
    val data = errors.json(error).getOrElse(errors.encode(error).status.text)
    ServerSentEvent(data, event = Some(SseField("error")))
end Subscription

/** One bound subscription. Its route runs the gate, then pulls that listener's stream. */
final class BoundSubscription[-R, In, Err, A](
    val subscription: Subscription[In, Err, A],
    val open: In => ZIO[R, Err, ZStream[R, Err, Observed[A]]],
    inner: Routes[R, Response],
) extends Routes[R, Response]:
  def input(request: Request): IO[Response, In] =
    subscription.endpoint.path.matches(request.path) match
      case None          => ZIO.fail(Response.notFound())
      case Some(matched) => subscription.endpoint.decodeIn(matched, request)

  def toChunk = inner.toChunk

  def apply(request: Request): ZIO[R, Response, Response] = inner(request)

  override private[heddle] def decide(request: Request): ZIO[R, Response, Outcome] =
    inner.decide(request)
end BoundSubscription
