package heddle

import java.nio.charset.StandardCharsets
import heddle.sse.SseCodec
import zio.*
import zio.stream.ZStream
import zio.test.*

final case class Tick(n: Int) derives Schema, JsonCodec

enum Miss derives Schema, JsonCodec:
  case Gone

object SubscriptionSpec extends ZIOSpecDefault:
  private val ticks =
    Endpoint.get("ticks").outError[Miss](Status.NotFound).stream[Tick]

  private def api(open: Unit => ZIO[Any, Miss, ZStream[Any, Miss, Observed[Tick]]]) =
    Api("ticks", "1").stream(ticks)(open)

  def spec = suite("Subscription")(
    test("a gate failure is the error status and not an event stream"):
      val routes = api(_ => ZIO.fail(Miss.Gone)).routes
      routes(Request.get("/ticks")).map { res =>
        assertTrue(
          res.status == Status.NotFound,
          !res.headers.contentType.exists(_.isEventStream),
          res.body.text.exists(_.contains("Gone")),
        )
      }
    ,
    test("an open stream with no element yet is 200, and a heartbeat follows"):
      for
        quiet <- Queue.unbounded[Observed[Tick]]
        routes = api(_ => ZIO.succeed(ZStream.fromQueue(quiet))).routes
        res   <- routes(Request.get("/ticks"))
        heard <- Queue.unbounded[Int]
        _     <- res.body.toStream.chunks
          .scan(Chunk.empty[Byte])(_ ++ _)
          .map(bytes => new String(bytes.toArray, StandardCharsets.US_ASCII).split(": ping", -1).length - 1)
          .filter(_ > 0)
          .changes
          .foreach(heard.offer)
          .fork
        first  <- TestClock.adjust(Subscription.Heartbeat) *> heard.take
        second <- TestClock.adjust(Subscription.Heartbeat) *> heard.take
      yield assertTrue(
        res.status == Status.Ok,
        res.headers.contentType.exists(_.isEventStream),
        res.header("Cache-Control").contains("no-cache"),
        first == 1,
        second == 2,
        ticks.doc.responses.exists(_.contentType.contains(MediaType.EventStream)),
      )
    ,
    test("the application id is the SSE id and a later failure is one error event"):
      val routes = api { _ =>
        ZIO.succeed(ZStream.succeed(Observed(SseField.of(4), Tick(4))) ++ ZStream.fail(Miss.Gone))
      }.routes
      routes(Request.get("/ticks")).flatMap(_.body.collect).map { bytes =>
        val (events, rest) = SseCodec.decode(bytes)
        val fact           = events.lift(0)
        val failed         = events.lift(1)
        assertTrue(
          rest.isEmpty,
          events.length == 2,
          fact.flatMap(_.id.map(_.value)).contains("4"),
          fact.map(_.data).contains("""{"n":4}"""),
          failed.flatMap(_.event.map(_.value)).contains(Subscription.ErrorEvent),
          failed.exists(_.data.contains("Gone")),
        )
      }
    ,
    test("two listeners are two pulls, and closing one runs only its finalizer"):
      for
        hub    <- Hub.unbounded[Observed[Tick]]
        opens  <- Ref.make(0)
        closed <- Ref.make(0)
        ready  <- Queue.unbounded[Unit]
        routes = api { _ =>
          opens.update(_ + 1) *> ZIO.succeed(
            ZStream
              .scoped(hub.subscribe.tap(_ => ready.offer(())))
              .flatMap(queue => ZStream.fromQueue(queue).ensuring(closed.update(_ + 1)))
          )
        }.routes
        pull = routes(Request.get("/ticks")).flatMap { res =>
          SseCodec.stream(res.body.toStream).filter(_.id.isDefined).foreach(_ => ZIO.unit)
        }
        scope1 <- Scope.make
        _      <- pull.forkIn(scope1)
        _      <- ready.take
        scope2 <- Scope.make
        heard  <- Queue.unbounded[String]
        _      <- routes(Request.get("/ticks"))
          .flatMap { res =>
            SseCodec.stream(res.body.toStream).foreach { event =>
              event.id match
                case Some(id) => heard.offer(id.value)
                case None     => ZIO.unit
            }
          }
          .forkIn(scope2)
        _      <- ready.take
        _      <- hub.publish(Observed(SseField.of(1), Tick(1)))
        first  <- heard.take
        _      <- scope1.close(Exit.unit)
        gone   <- closed.get
        _      <- hub.publish(Observed(SseField.of(2), Tick(2)))
        second <- heard.take
        nOpen  <- opens.get
        _      <- scope2.close(Exit.unit)
      yield assertTrue(nOpen == 2, gone == 1, first == "1", second == "2")
    ,
    test("Client.listen yields the facts and drops heartbeat comments"):
      val routes = api(_ => ZIO.succeed(ZStream.succeed(Observed(SseField.of(7), Tick(7))))).routes
      Client
        .listen(ticks)(())
        .runCollect
        .provideLayer(Client.inMemory(routes))
        .map(got => assertTrue(got == Chunk(Observed(SseField.of(7), Tick(7)))))
    ,
    test("Client.call does not accept a subscription"):
      typeCheck("""Client.call(Endpoint.get("ticks").stream[Tick])(())""").map { result =>
        assertTrue(result.isLeft)
      }
    ,
    test("marking a subscription promoted keeps the flag for the host to refuse"):
      assertTrue(Endpoint.get("ticks").mcp.stream[Tick].doc.promoted),
  )
end SubscriptionSpec
