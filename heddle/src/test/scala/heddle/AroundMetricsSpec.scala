package heddle

import zio.*
import zio.metrics.{Metric, MetricLabel}
import zio.test.*

/** Each test names its own metrics: `zio.metrics` is one registry for the whole run. */
object AroundMetricsSpec extends ZIOSpecDefault:
  private def names(prefix: String) =
    Around.MetricNames(s"${prefix}_total", s"${prefix}_in_flight", s"${prefix}_seconds")

  private def labels(pairs: (String, String)*): Set[MetricLabel] =
    pairs.map(MetricLabel(_, _)).toSet

  private def count(names: Around.MetricNames, tags: Set[MetricLabel]) =
    Metric.counterInt(names.requestsTotal).tagged(tags).value.map(_.count)

  private def inFlight(names: Around.MetricNames, tags: Set[MetricLabel]) =
    Metric.gauge(names.concurrentRequests).tagged(tags).value.map(_.value)

  private def duration(names: Around.MetricNames, tags: Set[MetricLabel]) =
    Metric.histogram(names.requestDuration, Around.defaultBoundaries).tagged(tags).value

  private val users =
    Routes(Method.GET / "users" / int("id") -> { id => ZIO.succeed(Response.text(id.toString)) })

  def spec =
    suite("Around.metrics")(
      test("a server counts and times each request by method, route template, and status"):
        val n     = names("server_ok")
        val route = labels("method" -> "GET", "path" -> "/users/{id}")
        val app   = users @@ Middleware.metrics(n)
        for
          _     <- app(Request.get("/users/1")) *> app(Request.get("/users/2"))
          total <- count(n, route + MetricLabel("status", "200"))
          live  <- inFlight(n, route)
          hist  <- duration(n, route + MetricLabel("status", "200"))
        yield assertTrue(total == 2.0, live == 0.0, hist.count == 2L)
      ,
      test("the duration is the time the route took"):
        val n   = names("server_time")
        val app = Routes(Method.GET / "slow" -> handler(TestClock.adjust(250.millis).as(Response.text("done")))) @@
          Middleware.metrics(n)
        for
          _    <- app(Request.get("/slow"))
          hist <- duration(n, labels("method" -> "GET", "path" -> "/slow", "status" -> "200"))
        yield assertTrue(hist.sum == 0.25, hist.max == 0.25)
      ,
      test("a failure that is a response reports that response's status"):
        val n   = names("server_404")
        val app = Routes(Method.GET / "gone" -> handler(ZIO.fail(Response.notFound("gone")))) @@ Middleware.metrics(n)
        for
          res   <- app(Request.get("/gone")).merge
          total <- count(n, labels("method" -> "GET", "path" -> "/gone", "status" -> "404"))
        yield assertTrue(res.status == Status.NotFound, total == 1.0)
      ,
      test("any other failure reports failed"):
        val n   = names("server_failed")
        val app = Routes(Method.GET / "boom" -> handler(ZIO.fail("boom"))) @@ Middleware.metrics(n)
        for
          out   <- app(Request.get("/boom")).either
          total <- count(n, labels("method" -> "GET", "path" -> "/boom", "status" -> "failed"))
        yield assertTrue(out == Left("boom"), total == 1.0)
      ,
      test("an interrupted request leaves the in-flight gauge where it started"):
        val n     = names("server_interrupt")
        val route = labels("method" -> "GET", "path" -> "/hang")
        for
          started <- Promise.make[Nothing, Unit]
          app = Routes(Method.GET / "hang" -> handler(started.succeed(()) *> ZIO.never)) @@ Middleware.metrics(n)
          fiber  <- app(Request.get("/hang")).fork
          _      <- started.await
          during <- inFlight(n, route)
          _      <- fiber.interrupt
          after  <- inFlight(n, route)
          total  <- count(n, route + MetricLabel("status", "failed"))
        yield assertTrue(during == 1.0, after == 0.0, total == 1.0)
      ,
      test("a client counts by method, host, and status under the client names"):
        val n = names("client_ok")
        for
          _ <- ZIO
            .serviceWithZIO[Client](_ @@ Around.metrics(client = n))
            .flatMap(_.batched(Request.get("http://api.example/users/3")))
            .provideLayer(Client.inMemory(users))
          total <- count(n, labels("method" -> "GET", "host" -> "api.example", "status" -> "200"))
        yield assertTrue(total == 1.0)
      ,
      test("extra labels ride on every metric"):
        val n     = names("server_extra")
        val extra = MetricLabel("service", "users")
        val app   = users @@ Middleware.metrics(n, extraLabels = Set(extra))
        for
          _     <- app(Request.get("/users/1"))
          total <- count(n, labels("method" -> "GET", "path" -> "/users/{id}", "status" -> "200") + extra)
        yield assertTrue(total == 1.0),
    )
end AroundMetricsSpec
