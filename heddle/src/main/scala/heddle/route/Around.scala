package heddle.route

import heddle.http.{Method, Request, Response}
import zio.*
import zio.metrics.{Metric, MetricKeyType, MetricLabel}

/** Which end of an exchange middleware runs on. */
enum Side:
  case Server, Client

/** The route a server matched: its method and path template. A template has no ids in it, so it is safe as a metric
  * label or a span name.
  */
final case class RouteName(method: Method, template: String):
  def render: String = s"${method.render} $template"

/** One request and its response, as middleware sees it. On a server, `route` is the route that answers; a client has no
  * route.
  */
final case class Exchange(side: Side, request: Request, route: Option[RouteName])

/** Middleware for an exchange, applied the same way to a server (`routes @@ Middleware.around(a)`) or a client (`client @@
  * a`). `next` carries the request on, to the route or to the network, and may be handed a changed one: a client adds a
  * trace header, a server reads it first.
  */
trait Around[-R]:
  self =>
  def apply[R1 <: R, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response]

  /** `this` runs outside `that`. */
  def ++[R1 <: R](that: Around[R1]): Around[R1] =
    new Around[R1]:
      def apply[R2 <: R1, E](exchange: Exchange)(next: Request => ZIO[R2, E, Response]): ZIO[R2, E, Response] =
        self(exchange)(req => that(exchange.copy(request = req))(next))
end Around

object Around:
  val identity: Around[Any] =
    new Around[Any]:
      def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response] =
        next(exchange.request)

  /** The three metrics each side keeps. */
  final case class MetricNames(requestsTotal: String, concurrentRequests: String, requestDuration: String)

  object MetricNames:
    /** zio-http's names, so a server's dashboards carry over. */
    val server: MetricNames =
      MetricNames("http_requests_total", "http_concurrent_requests_total", "http_request_duration_seconds")

    val client: MetricNames =
      MetricNames(
        "http_client_requests_total",
        "http_client_concurrent_requests_total",
        "http_client_request_duration_seconds",
      )
  end MetricNames

  val defaultBoundaries: MetricKeyType.Histogram.Boundaries =
    MetricKeyType.Histogram.Boundaries.fromChunk(
      Chunk(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1.0, 2.5, 5.0, 7.5, 10.0)
    )

  /** A request count, an in-flight gauge, and a duration histogram in seconds, through `zio.metrics`.
    *
    * Labels are `method`, `status`, and where the request went: `path` (the route's template) on a server and `host` on
    * a client, never a raw path. `status` is the response's code. An effect that fails with a `Response` reports that
    * response's code; any other failure reports `failed`.
    */
  def metrics(
      server: MetricNames = MetricNames.server,
      client: MetricNames = MetricNames.client,
      boundaries: MetricKeyType.Histogram.Boundaries = defaultBoundaries,
      extraLabels: Set[MetricLabel] = Set.empty,
  ): Around[Any] =
    new Around[Any]:
      def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]): ZIO[R1, E, Response] =
        val names = exchange.side match
          case Side.Server => server
          case Side.Client => client
        val where = exchange.route match
          case Some(route) => MetricLabel("path", route.template)
          case None        => MetricLabel("host", exchange.request.url.host.getOrElse(""))
        val request  = extraLabels + MetricLabel("method", exchange.request.method.render) + where
        val inFlight = Metric.gauge(names.concurrentRequests).tagged(request)
        def done(status: String, nanos: Long): UIO[Unit] =
          val labels = request + MetricLabel("status", status)
          Metric.counterInt(names.requestsTotal).tagged(labels).increment *>
            Metric.histogram(names.requestDuration, boundaries).tagged(labels).update(nanos / 1e9d)
        ZIO.uninterruptibleMask { restore =>
          for
            start    <- Clock.nanoTime
            _        <- inFlight.increment
            response <- restore(next(exchange.request)).onExit { exit =>
              Clock.nanoTime.flatMap { end =>
                val status = exit match
                  case Exit.Success(response) => response.status.code.toString
                  case Exit.Failure(cause)    =>
                    cause.failureOption match
                      case Some(response: Response) => response.status.code.toString
                      case _                        => "failed"
                inFlight.decrement *> done(status, end - start)
              }
            }
          yield response
        }
end Around
