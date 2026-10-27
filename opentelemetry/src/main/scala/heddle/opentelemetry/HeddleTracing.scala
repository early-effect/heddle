package heddle.opentelemetry

import heddle.http.{Request, Response}
import heddle.route.{Around, Exchange, Side}
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.api.trace.{SpanKind, StatusCode}
import scala.collection.mutable
import zio.*
import zio.telemetry.opentelemetry.context.{IncomingContextCarrier, OutgoingContextCarrier}
import zio.telemetry.opentelemetry.tracing.{StatusMapper, Tracing}
import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator

/** OpenTelemetry spans for heddle exchanges: one [[Around]] for a server (`routes @@ Middleware.around(...)`) and a
  * client (`client @@ ...`).
  *
  * A server span is `SERVER`, named for the matched route (`POST /users/{id}`), and continues the caller's trace from
  * its `traceparent`. A client span is `CLIENT`, named for the method, and sends its own context on as `traceparent`.
  * Both use OpenTelemetry's HTTP attribute names. A server marks a 5xx as an error, a client a 4xx or 5xx, and either
  * marks a failed effect.
  */
object HeddleTracing:
  def around(tracing: Tracing, propagator: TraceContextPropagator = TraceContextPropagator.default): Around[Any] =
    new Around[Any]:
      def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response] =
        exchange.side match
          case Side.Server => server(tracing, propagator, exchange, next)
          case Side.Client => client(tracing, propagator, exchange, next)

  /** [[around]] with the `Tracing` in the environment. */
  val live: URIO[Tracing, Around[Any]] =
    ZIO.serviceWith[Tracing](around(_))

  private def server[R, E](
      tracing: Tracing,
      propagator: TraceContextPropagator,
      exchange: Exchange,
      next: Request => ZIO[R, E, Response],
  ): ZIO[R, E, Response] =
    val req        = exchange.request
    val attributes = Attributes.builder()
    attributes.put("http.request.method", req.method.render)
    attributes.put("url.path", req.path.render)
    exchange.route.foreach(route => attributes.put("http.route", route.template))
    val headers = mutable.Map.from(req.headers.toChunk.map(h => h.name.render.toLowerCase -> h.value))
    tracing.extractSpan(
      propagator,
      IncomingContextCarrier.default(headers),
      exchange.route.fold(req.method.render)(_.render),
      SpanKind.SERVER,
      attributes.build(),
      errorsFrom(500),
    )(recorded(tracing, next(req)))

  private def client[R, E](
      tracing: Tracing,
      propagator: TraceContextPropagator,
      exchange: Exchange,
      next: Request => ZIO[R, E, Response],
  ): ZIO[R, E, Response] =
    val req        = exchange.request
    val attributes = Attributes.builder()
    attributes.put("http.request.method", req.method.render)
    attributes.put("url.full", req.url.render)
    req.url.host.foreach(host => attributes.put("server.address", host))
    tracing.span(req.method.render, SpanKind.CLIENT, attributes.build(), errorsFrom(400)) {
      ZIO.succeed(OutgoingContextCarrier.default()).flatMap { carrier =>
        tracing.injectSpan(propagator, carrier) *>
          recorded(tracing, next(carrier.kernel.foldLeft(req) { case (r, (k, v)) => r.withHeader(k, v) }))
      }
    }

  /** The status lands on the span whether the effect answered or failed with a response. */
  private def recorded[R, E](tracing: Tracing, run: ZIO[R, E, Response]): ZIO[R, E, Response] =
    def status(response: Response) = tracing.setAttribute("http.response.status_code", response.status.code.toLong)
    run.tapBoth(
      {
        case response: Response => status(response)
        case _                  => ZIO.unit
      },
      status,
    )

  private def errorsFrom(code: Int): StatusMapper[Any, Response] =
    StatusMapper.both(
      StatusMapper.failureNoException[Any](_ => StatusCode.ERROR),
      StatusMapper.Success[Response] {
        case response if response.status.code >= code => StatusMapper.Result(StatusCode.ERROR)
      },
    )
end HeddleTracing
