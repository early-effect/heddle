package heddle.opentelemetry

import heddle.*
import io.opentelemetry.api.common.AttributeKey
import io.opentelemetry.api.trace.{SpanKind, StatusCode, Tracer}
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.data.SpanData
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import scala.jdk.CollectionConverters.*
import zio.*
import zio.telemetry.opentelemetry.OpenTelemetry
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.test.*

object HeddleTracingSpec extends ZIOSpecDefault:
  private val exporter: ULayer[InMemorySpanExporter & Tracer] =
    ZLayer.fromZIOEnvironment(ZIO.succeed {
      val spans    = InMemorySpanExporter.create()
      val provider = SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(spans)).build()
      ZEnvironment(spans).add(provider.get("HeddleTracingSpec"))
    })

  private val tracing: ULayer[Tracing & InMemorySpanExporter] =
    ZLayer.make[Tracing & InMemorySpanExporter](exporter, Tracing.live(), OpenTelemetry.contextZIO)

  private val finished: URIO[InMemorySpanExporter, Chunk[SpanData]] =
    ZIO.serviceWith[InMemorySpanExporter](spans => Chunk.fromIterable(spans.getFinishedSpanItems.asScala))

  private def string(span: SpanData, key: String): Option[String] =
    Option(span.getAttributes.get(AttributeKey.stringKey(key)))

  private def long(span: SpanData, key: String): Option[Long] =
    Option(span.getAttributes.get(AttributeKey.longKey(key))).map(_.longValue)

  private val traceId  = "4bf92f3577b34da6a3ce929d0e0e4736"
  private val parentId = "00f067aa0ba902b7"

  private val users =
    Routes(
      Method.GET / "users" / int("id") -> { id => ZIO.succeed(Response.text(id.toString)) },
      Method.GET / "broken"            -> Handler.succeed(Response.internalServerError()),
      Method.GET / "parent"            -> handler { (req: Request) =>
        ZIO.succeed(Response.text(req.header("traceparent").getOrElse("")))
      },
    )

  private def traced[R, E](routes: Routes[R, E]): URIO[Tracing, Routes[R, E]] =
    HeddleTracing.live.map(around => routes @@ Middleware.around(around))

  private val server =
    suite("on a server")(
      test("continues the caller's trace in a SERVER span named for the route"):
        for
          app   <- traced(users)
          res   <- app(Request.get("/users/7").withHeader("traceparent", s"00-$traceId-$parentId-01"))
          spans <- finished
        yield assertTrue(
          res.status == Status.Ok,
          spans.map(_.getName) == Chunk("GET /users/{id}"),
          spans.forall(_.getKind == SpanKind.SERVER),
          spans.forall(_.getTraceId == traceId),
          spans.forall(_.getParentSpanId == parentId),
          spans.flatMap(string(_, "http.route")) == Chunk("/users/{id}"),
          spans.flatMap(string(_, "url.path")) == Chunk("/users/7"),
          spans.flatMap(string(_, "http.request.method")) == Chunk("GET"),
          spans.flatMap(long(_, "http.response.status_code")) == Chunk(200L),
          spans.forall(_.getStatus.getStatusCode == StatusCode.UNSET),
        )
      ,
      test("starts a new trace when no traceparent arrives"):
        for
          app   <- traced(users)
          _     <- app(Request.get("/users/7"))
          spans <- finished
        yield assertTrue(spans.size == 1, spans.forall(s => s.getTraceId != traceId && !s.getParentSpanContext.isValid))
      ,
      test("a 5xx is an error"):
        for
          app   <- traced(users)
          _     <- app(Request.get("/broken"))
          spans <- finished
        yield assertTrue(
          spans.flatMap(long(_, "http.response.status_code")) == Chunk(500L),
          spans.forall(_.getStatus.getStatusCode == StatusCode.ERROR),
        )
      ,
      test("a failed effect is an error and still records the response it failed with"):
        for
          app   <- traced(Routes(Method.GET / "gone" -> handler(ZIO.fail(Response.empty(Status.ServiceUnavailable)))))
          _     <- app(Request.get("/gone")).merge
          spans <- finished
        yield assertTrue(
          spans.flatMap(long(_, "http.response.status_code")) == Chunk(503L),
          spans.forall(_.getStatus.getStatusCode == StatusCode.ERROR),
        )
      ,
      test("a miss opens no span"):
        for
          app   <- traced(users)
          res   <- app(Request.get("/nowhere"))
          spans <- finished
        yield assertTrue(res.status == Status.NotFound, spans.isEmpty),
    )

  private val client =
    suite("on a client")(
      test("sends its CLIENT span's context as traceparent"):
        for
          around <- HeddleTracing.live
          res    <- ZIO
            .serviceWithZIO[Client](_ @@ around)
            .flatMap(_.batched(Request.get("http://api.example/parent")))
            .provideSomeLayer[Tracing](Client.inMemory(users))
          spans <- finished
        yield assertTrue(
          spans.map(_.getName) == Chunk("GET"),
          spans.forall(_.getKind == SpanKind.CLIENT),
          spans.flatMap(string(_, "server.address")) == Chunk("api.example"),
          spans.flatMap(string(_, "url.full")) == Chunk("http://api.example/parent"),
          spans.map(s => s"00-${s.getTraceId}-${s.getSpanId}-${s.getSpanContext.getTraceFlags.asHex}") ==
            Chunk(res.body.text.getOrElse("")),
        )
      ,
      test("a 4xx is an error"):
        for
          around <- HeddleTracing.live
          _      <- ZIO
            .serviceWithZIO[Client](_ @@ around)
            .flatMap(_.batched(Request.get("http://api.example/nowhere")))
            .provideSomeLayer[Tracing](Client.inMemory(users))
          spans <- finished
        yield assertTrue(
          spans.flatMap(long(_, "http.response.status_code")) == Chunk(404L),
          spans.forall(_.getStatus.getStatusCode == StatusCode.ERROR),
        ),
    )

  def spec =
    suite("HeddleTracing")(
      server,
      client,
      test("a traced client calling a traced server makes one trace, client span as parent"):
        for
          around <- HeddleTracing.live
          _      <- ZIO
            .serviceWithZIO[Client](_.batched(Request.get("http://api.example/users/7")))
            .provideSomeLayer[Tracing](
              Client.inMemory(users @@ Middleware.around(around)) >>> Client.around(around)
            )
          spans <- finished
          byKind = spans.groupBy(_.getKind)
        yield assertTrue(
          spans.size == 2,
          byKind.get(SpanKind.SERVER).exists(_.forall(_.getName == "GET /users/{id}")),
          spans.map(_.getTraceId).distinct.size == 1,
          byKind
            .get(SpanKind.SERVER)
            .exists(server =>
              byKind.get(SpanKind.CLIENT).exists(client => server.map(_.getParentSpanId) == client.map(_.getSpanId))
            ),
        ),
    ).provide(tracing)
end HeddleTracingSpec
