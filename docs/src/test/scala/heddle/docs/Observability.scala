package heddle.docs

import heddle.*
import heddle.opentelemetry.HeddleTracing
import io.opentelemetry.api.trace.SpanKind
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import scala.jdk.CollectionConverters.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.*
import zio.metrics.{Metric, MetricLabel}
import zio.telemetry.opentelemetry.OpenTelemetry
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.test.*

object Observability extends DocSpecSuite:
  private val server = Around.MetricNames.server
  private val client = Around.MetricNames.client

  def doc = page("Observability")(
    md"""
Metrics and traces are middleware, and the same middleware runs at either end of a call. An `Around`
sees one `Exchange`: which `Side` it is on, the `Request`, and on a server the route that matched.
`routes @@ Middleware.around(a)` runs it on a server. `client @@ a` (or the `Client.around(a)`
layer) runs it on a client. `a ++ b` runs `a` outside `b`.

A server hands an `Around` the route's template (`/users/{id}`), never the raw path, so a label or a
span name cannot grow one series per user. A request no route matched is a miss, and a miss is not
an exchange: the middleware never sees it, and a later `++` still gets its turn. A 405 is the same.
""",
    section("One value, both ends")(
      md"""
`next` carries the request on, to the route or to the network, and it may be handed a changed one.
This `Around` stamps a header and records where it ran. The client and the server it calls each run
it once.
""",
      exampleZIO {
        for
          seen <- Ref.make(Chunk.empty[(Side, Option[String])])
          stamp = new Around[Any]:
            def apply[R1, E](exchange: Exchange)(next: Request => ZIO[R1, E, Response]) =
              seen.update(_ :+ (exchange.side -> exchange.route.map(_.render))) *>
                next(exchange.request.withHeader("X-Stamped", "yes"))
          users = Routes(
            Method.GET / "users" / int("id") handle { (id, req) =>
              ZIO.succeed(Response.text(s"$id ${req.header("X-Stamped").getOrElse("no")}"))
            }
          ) @@ Middleware.around(stamp)
          body <- ZIO
            .serviceWithZIO[Client](_.batched(Request.get("http://users.internal/users/7")))
            .flatMap(_.body.utf8)
            .provideLayer(Client.inMemory(users) >>> Client.around(stamp))
          all <- seen.get
        yield (body, all)
      }.assert { case (body, all) =>
        assertTrue(
          body == "7 yes",
          all == Chunk(Side.Client -> None, Side.Server -> Some("GET /users/{id}")),
        )
      },
    ),
    section("Metrics")(
      md"""
`Middleware.metrics()` keeps zio-http's three server metrics through `zio.metrics`, so a dashboard
built for zio-http reads a heddle server unchanged: `${server.requestsTotal}` (a counter),
`${server.concurrentRequests}` (a gauge of requests in flight), and `${server.requestDuration}` (a
histogram in seconds). Labels are `method`, `path` (the template), and `status`.

A client keeps the same three under `${client.requestsTotal}`, `${client.concurrentRequests}`, and
`${client.requestDuration}`, with `host` in place of `path`. `Around.metrics()` is one value for
both: it picks the names by `Side`.

`status` is the response's code. An effect that fails with a `Response` reports that response's
code, and any other failure (a `ClientError`, a typed domain error, an interruption) reports
`failed`. The in-flight gauge comes back down on every exit, interruption included.
""",
      exampleZIO {
        val users    = Routes(Method.GET / "users" / int("id") -> { id => ZIO.succeed(Response.text(id.toString)) })
        val app      = users @@ Middleware.metrics()
        val requests = Metric
          .counterInt(server.requestsTotal)
          .tagged(MetricLabel("method", "GET"), MetricLabel("path", "/users/{id}"), MetricLabel("status", "200"))
        for
          before <- requests.value
          _      <- app(Request.get("/users/1")) *> app(Request.get("/users/2"))
          after  <- requests.value
        yield after.count - before.count
      }.assert(counted => assertTrue(counted == 2.0)),
      md"""
`extraLabels` rides on every metric (a service name, a region). `boundaries` replaces the histogram
buckets. `Around.MetricNames` renames all three when two servers share one registry.
""",
    ),
    section("Tracing")(
      md"""
OpenTelemetry is JVM-only, so spans are their own artifact:

```scala
libraryDependencies += "rocks.earlyeffect" %% "heddle-opentelemetry" % "${DocsVersion.version}"
```

`HeddleTracing.around(tracing)` is an `Around` over zio-opentelemetry's `Tracing`, and
`HeddleTracing.live` reads it from the environment. A server opens a `SERVER` span named for the
route (`GET /users/{id}`) and continues the caller's trace from its `traceparent`. A client opens a
`CLIENT` span named for the method and sends its context on as `traceparent`. Spans carry
OpenTelemetry's HTTP attribute names: `http.request.method`, `http.route`, `url.path`, `url.full`,
`server.address`, and `http.response.status_code`. A server marks a 5xx as an error, a client a 4xx
or a 5xx, and either one marks an effect that failed.

Here a traced client calls a traced server. The two spans are one trace, and the server's parent is
the client's span. The exporter is in memory so the page can read it; in production `Tracing` comes
from your OpenTelemetry SDK or agent.
""",
      exampleZIO {
        val spans    = InMemorySpanExporter.create()
        val provider = SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(spans)).build()
        val tracing  = ZLayer.make[Tracing](
          ZLayer.succeed(provider.get("docs")),
          Tracing.live(),
          OpenTelemetry.contextZIO,
        )
        val users = Routes(Method.GET / "users" / int("id") -> { id => ZIO.succeed(Response.text(id.toString)) })
        val call  =
          for
            traced <- HeddleTracing.live
            _      <- ZIO
              .serviceWithZIO[Client](_.batched(Request.get("http://users.internal/users/7")))
              .provideSomeLayer[Tracing](
                Client.inMemory(users @@ Middleware.around(traced)) >>> Client.around(traced)
              )
          yield Chunk.fromIterable(spans.getFinishedSpanItems.asScala)
        call.provideLayer(tracing)
      }.assert { finished =>
        val byKind = finished.groupBy(_.getKind)
        assertTrue(
          finished.map(s => s.getKind -> s.getName).toSet == Set(
            SpanKind.SERVER -> "GET /users/{id}",
            SpanKind.CLIENT -> "GET",
          ),
          finished.map(_.getTraceId).distinct.size == 1,
          byKind.get(SpanKind.SERVER).map(_.map(_.getParentSpanId)) == byKind
            .get(SpanKind.CLIENT)
            .map(_.map(_.getSpanId)),
        )
      },
    ),
    section("Order")(
      md"""
Apply tracing outside metrics when both are on, so the span covers the time the metric records:
`routes @@ Middleware.around(HeddleTracing.around(tracing) ++ Around.metrics())`. On a client the
same value goes on `client @@`.
"""
    ),
  )
end Observability
