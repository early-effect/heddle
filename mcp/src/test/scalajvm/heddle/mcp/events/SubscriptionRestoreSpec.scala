package heddle.mcp.events

import heddle.*
import heddle.mcp.Mcp
import heddle.mcp.protocol.{ClientRequest, DeliveryHeaders, EventName, RpcError, TtlRequest}
import slayer.*
import zio.*
import zio.durationInt
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.jdk.CollectionConverters.*

object SubscriptionRestoreSpec extends ZIOSpecDefault:
  private val Start       = Instant.parse("2026-11-26T12:00:00Z")
  private inline val Name = "session.milestone"
  private val Url         = "https://hooks.example/events"
  private val SecretBytes = Chunk.fill(32)(7.toByte)
  private val Secret      = whsec(7)
  private val Other       = whsec(9)

  private val policy: TtlPolicy = TtlPolicy.sanitize(
    TtlPolicy.default.copy(floor = 1.second, default = 1.minute, cap = 1.hour)
  )

  private def whsec(fill: Byte): String =
    "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(fill))

  private def args: Json.Obj = Json.Obj("kind" -> Json.Str("edit"))

  private def entry(id: String, until: Instant, last: Option[Instant]): StoredSubscription =
    StoredSubscription.trusted(id, "grok", Url, Name, args, until, SecretBytes, DeliveryHeaders.empty, last)

  private def scratch: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt {
        val root = Path.of("target")
        Files.createDirectories(root)
        Files.createTempDirectory(root, "subscription-store")
      }
    )(root => ZIO.attempt(deleteTree(root)).catchAll(_ => ZIO.unit))

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walked = Files.walk(root)
      try
        val paths = walked.iterator.asScala.toVector.sortBy(_.getNameCount)(using Ordering[Int].reverse)
        paths.foreach(path => Files.deleteIfExists(path))
      finally walked.close()

  private def ids(snapshot: EventSnapshot): Chunk[String] =
    Chunk.fromIterable(snapshot.subscriptions.map(_.id))

  private def server(use: TtlPolicy): Either[NonEmptyChunk[heddle.mcp.McpBuildError], Mcp[Events.Env]] =
    Mcp
      .from(Api("Shop", "1.0.0"))
      .flatMap {
        _.event[Filter, Milestone](Name, "A session milestone").using(
          NonEmptyChunk(Delivery.Poll, Delivery.Webhook),
          Replay.default,
        )((filter, milestone) => filter.kind == milestone.kind)
      }
      .map(
        _.principal(_ => ZIO.succeed(Some(Principal("grok"))))
          .eventPolicy(use)
          .verificationPolicy(VerificationPolicy.Skip)
      )

  private def run[A](mcp: Mcp[Events.Env], path: Path)(
      program: ZIO[Events.Env & Stubbed[WebhookTransport], Any, A]
  ): ZIO[Any, Any, A] =
    program.provide(mcp.eventsLayer(SubscriptionStore.file(path.toString, 32)) ++ stubbed[WebhookTransport])

  private def subscribe(url: String, secret: String): URIO[Events.Env, Either[RpcError, Json.Obj]] =
    ZIO.serviceWithZIO[Events](
      _.subscribe(
        Some(Principal("grok")),
        ClientRequest.SubscribeEvents(
          EventName(Name),
          args,
          url,
          secret,
          None,
          None,
          TtlRequest.Absent,
          DeliveryHeaders.empty,
        ),
      )
    )

  private def poll: URIO[Events.Env, Either[RpcError, Json.Obj]] =
    ZIO.serviceWithZIO[Events](_.poll(None, ClientRequest.PollEvents(EventName(Name), args, None, None, None)))

  private def emit(id: String): ZIO[Events.Env, Any, Unit] =
    for
      at   <- Clock.instant
      name <- ZIO.fromEither(EventId.from(id))
      _    <- Events.publish(EventName(Name), name, Milestone("edit", id), at)
    yield ()

  private def text(obj: Json.Obj, key: String): Option[String] =
    obj.get(key).collect { case Json.Str(value) => value }

  private def readSnap(path: Path): ZIO[Any, Any, EventSnapshot] =
    ZIO
      .attempt(Files.readString(path, StandardCharsets.UTF_8))
      .flatMap(raw => ZIO.fromEither(EventSnapshot.decode(raw)))

  private def mac(secret: Chunk[Byte], id: String, timestamp: String, body: String): String =
    val hmac = Mac.getInstance("HmacSHA256")
    hmac.init(new SecretKeySpec(secret.toArray, "HmacSHA256"))
    val raw = hmac.doFinal(s"$id.$timestamp.$body".getBytes(StandardCharsets.UTF_8))
    "v1," + Base64.getEncoder.encodeToString(raw)

  private def signed(call: WebhookCall, secret: String): Boolean =
    WebhookSecret.from(secret) match
      case Left(_)    => false
      case Right(key) =>
        (call.header("webhook-id"), call.header("webhook-timestamp"), call.header("webhook-signature")) match
          case (Some(id), Some(timestamp), Some(header)) =>
            header.split(' ').exists(_ == mac(key.bytes, id, timestamp, call.body))
          case _ => false

  override def spec = suite("subscription restore")(
    test("snapshot json keeps the secret and the last delivery") {
      val headers = DeliveryHeaders.parse(Json.Obj("Authorization" -> Json.Str("Bearer receiver-key")))
      val built   = headers.map { parsed =>
        val at = Instant.parse("2026-11-26T12:00:00Z")
        EventSnapshot(
          Vector(
            StoredSubscription.trusted(
              "sub_a",
              "grok",
              Url,
              Name,
              args,
              at.plusSeconds(60),
              SecretBytes,
              parsed,
              Some(at),
            )
          ),
          Vector(RingHead(Name, 2L)),
        )
      }
      built match
        case Left(_)         => assertTrue(false)
        case Right(snapshot) =>
          EventSnapshot.decode(EventSnapshot.encode(snapshot)) match
            case Left(_)     => assertTrue(false)
            case Right(back) =>
              val shown = snapshot.toString + back.toString
              assertTrue(
                back == snapshot,
                snapshot.subscriptions.flatMap(_.lastDeliveryAt).nonEmpty,
                !shown.contains("whsec_"),
                !shown.contains("receiver-key"),
              )
    },
    test("closing the owning scope drops the subscription before its refresh time") {
      for
        at <- Clock.instant
        row = entry("sub_scope", at.plusSeconds(3600), None)
        mid   <- ZIO.scoped(ZIO.serviceWithZIO[SubscriptionStore](store => store.retain(row) *> store.snapshot))
        after <- ZIO.serviceWithZIO[SubscriptionStore](_.snapshot)
      yield assertTrue(ids(mid).contains(row.id), !ids(after).contains(row.id))
    }.provide(SubscriptionStore.memory(4)),
    test("a refresh time that passes drops the subscription") {
      for
        at <- Clock.instant
        row = entry("sub_ttl", at.plusSeconds(60), None)
        store <- ZIO.service[SubscriptionStore]
        _     <- store.retain(row)
        _     <- TestClock.adjust(61.seconds)
        after <- store.snapshot
      yield assertTrue(!ids(after).contains(row.id))
    }.provideSome[Scope](SubscriptionStore.memory(4)),
    test("renew moves the refresh time and the new time still drops the row") {
      for
        at <- Clock.instant
        row = entry("sub_renew", at.plusSeconds(60), None)
        store <- ZIO.service[SubscriptionStore]
        _     <- store.retain(row)
        _     <- store.renew(row.id, at.plusSeconds(120))
        _     <- TestClock.adjust(61.seconds)
        mid   <- store.snapshot
        _     <- TestClock.adjust(60.seconds)
        after <- store.snapshot
      yield assertTrue(ids(mid).contains(row.id), !ids(after).contains(row.id))
    }.provideSome[Scope](SubscriptionStore.memory(4)),
    test("a new subscription at capacity is refused") {
      for
        at     <- Clock.instant
        store  <- ZIO.service[SubscriptionStore]
        _      <- store.retain(entry("sub_a", at.plusSeconds(60), None))
        second <- store.retain(entry("sub_b", at.plusSeconds(60), None)).either
        kept   <- store.snapshot
      yield assertTrue(second == Left(StoreError.Capacity(1)), ids(kept) == Chunk("sub_a"))
    }.provideSome[Scope](SubscriptionStore.memory(1)),
    test("closing the scope removes the row from the file") {
      ZIO.scoped {
        for
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          at <- Clock.instant
          row = entry("sub_file", at.plusSeconds(3600), None)
          _ <- ZIO
            .serviceWithZIO[SubscriptionStore] { store =>
              ZIO.scoped(store.retain(row))
            }
            .provide(SubscriptionStore.file(path.toString, 4))
          raw <- ZIO.attempt(Files.readString(path, StandardCharsets.UTF_8))
        yield assertTrue(!raw.contains(row.id))
      }
    },
    test("a kept subscription reloads from the file") {
      ZIO.scoped {
        for
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          at <- Clock.instant
          row = entry("sub_kept", at.plusSeconds(3600), None)
          _ <- ZIO
            .serviceWithZIO[SubscriptionStore] { store =>
              ZIO.scoped(store.retain(row) *> store.markKept(row.id))
            }
            .provide(SubscriptionStore.file(path.toString, 4))
          back <- ZIO.serviceWithZIO[SubscriptionStore](_.snapshot).provide(SubscriptionStore.file(path.toString, 4))
        yield assertTrue(ids(back).contains(row.id))
      }
    },
    test("loading a file drops expired, undecodable, and invalid heads") {
      ZIO.scoped {
        for
          _   <- TestClock.setTime(Start)
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          raw  =
            s"""{"subscriptions":[{"id":"sub_good","principal":"grok","url":"$Url","name":"$Name","arguments":{"kind":"edit"},"refreshBefore":"2026-11-26T13:00:00Z","secret":"$Secret"},{"id":"sub_old","principal":"grok","url":"$Url","name":"$Name","arguments":{"kind":"edit"},"refreshBefore":"2026-11-26T12:00:00Z","secret":"$Secret"},{"id":"sub_bad","principal":"grok","url":"$Url","name":"$Name","arguments":{"kind":"edit"},"refreshBefore":"2026-11-26T13:00:00Z","secret":"nope"}],"heads":[{"name":"$Name","head":4},{"name":"","head":9},{"name":"$Name","head":-1},{"name":"other","head":2}]}"""
          _    <- ZIO.attempt(Files.writeString(path, raw, StandardCharsets.UTF_8))
          back <- ZIO.serviceWithZIO[SubscriptionStore](_.snapshot).provide(SubscriptionStore.file(path.toString, 8))
          text <- ZIO.attempt(Files.readString(path, StandardCharsets.UTF_8))
          disk <- readSnap(path)
        yield assertTrue(
          ids(back) == Chunk("sub_good"),
          ids(disk) == Chunk("sub_good"),
          !text.contains("nope"),
          !text.contains("sub_bad"),
          !text.contains("sub_old"),
          disk.heads.exists(head => head.name == Name && head.head == 4L),
          disk.heads.exists(head => head.name == "other" && head.head == 2L),
          !disk.heads.exists(_.head < 0L),
        )
      }
    },
    test("a write drops an expired subscription") {
      ZIO.scoped {
        for
          _   <- TestClock.setTime(Start)
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          live = entry("sub_live", Start.plusSeconds(3600), None)
          dead = entry("sub_dead", Start.minusSeconds(1), None)
          _ <- ZIO
            .serviceWithZIO[SubscriptionStore](_.replace(EventSnapshot(Vector(live, dead), Vector(RingHead(Name, 3L)))))
            .provide(SubscriptionStore.file(path.toString, 8))
          disk <- readSnap(path)
        yield assertTrue(ids(disk) == Chunk("sub_live"), disk.heads.exists(_.head == 3L))
      }
    },
    test("a restart keeps the subscription id and serves the next event at head + 1") {
      ZIO.scoped {
        for
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          mcp <- ZIO.fromEither(server(policy)).mapError(_.mkString(","))
          _   <- TestClock.setTime(Start)
          id  <- run(mcp, path) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- stub[WebhookTransport](_.post(any[WebhookCall])) { (call: WebhookCall) =>
                calls.update(_ :+ call).as(DeliveryReply(200, ""))
              }
              subscribed <- subscribe(Url, Secret)
              _          <- emit("m1")
            yield subscribed
          }
          cursor <- run(mcp, path) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- stub[WebhookTransport](_.post(any[WebhookCall])) { (call: WebhookCall) =>
                calls.update(_ :+ call).as(DeliveryReply(200, ""))
              }
              polled <- poll
              _      <- emit("m2")
              seen   <- calls.get
            yield (polled, seen)
          }
        yield
          val subscribedId   = id.toOption.flatMap(obj => text(obj, "id"))
          val (polled, seen) = cursor
          val posted         = seen.headOption
          assertTrue(
            subscribedId.exists(_.startsWith("sub_")),
            polled.toOption.flatMap(obj => text(obj, "cursor")).contains("1"),
            posted.exists(call => call.header("X-MCP-Subscription-Id") == subscribedId),
            posted.exists(_.body.contains("m2")),
            posted.exists(call => !call.body.contains("\"gap\"")),
            posted.exists(call => signed(call, Secret)),
          )
      }
    },
    test("a restored subscription with no last delivery can be renewed, and a fresh one cannot") {
      ZIO.scoped {
        for
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          mcp <- ZIO.fromEither(server(policy)).mapError(_.mkString(","))
          _   <- TestClock.setTime(Start)
          _   <- run(mcp, path) {
            for
              _      <- subscribe(Url, Secret)
              _      <- Events.renewHealthy(5.minutes)
              before <- readSnap(path)
              renewed = before.subscriptions.headOption.map(_.refreshBefore)
            yield renewed
          }
          beforeRenew <- readSnap(path)
          _           <- run(mcp, path)(Events.renewHealthy(5.minutes))
          after       <- readSnap(path)
        yield
          val first  = beforeRenew.subscriptions.headOption
          val second = after.subscriptions.headOption
          assertTrue(
            first.exists(_.lastDeliveryAt.isEmpty),
            first.exists(_.refreshBefore == Start.plusSeconds(60)),
            second.exists(_.refreshBefore == Start.plus(5.minutes)),
            second.exists(_.lastDeliveryAt.isEmpty),
          )
      }
    },
    test("renewHealthy extends a delivered subscription and lets a failing url expire") {
      ZIO.scoped {
        for
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          mcp  <- ZIO.fromEither(server(policy)).mapError(_.mkString(","))
          _    <- TestClock.setTime(Start)
          seen <- run(mcp, path) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- stub[WebhookTransport](_.post(any[WebhookCall])) { (call: WebhookCall) =>
                val status = if call.url.contains("healthy") then 200 else 500
                calls.update(_ :+ call).as(DeliveryReply(status, ""))
              }
              healthy <- subscribe("https://hooks.example/healthy", Secret)
              failing <- subscribe("https://hooks.example/failing", Other)
              _       <- emit("m1")
              _       <- Events.renewHealthy(30.minutes)
              granted <- readSnap(path)
              _       <- TestClock.adjust(61.seconds)
              left    <- readSnap(path)
              _       <- calls.set(Chunk.empty)
              _       <- emit("m2")
              posted  <- calls.get
            yield (healthy, failing, granted, left, posted)
          }
        yield
          val (healthy, failing, granted, left, posted) = seen
          val healthyId                                 = healthy.toOption.flatMap(obj => text(obj, "id"))
          val failingId                                 = failing.toOption.flatMap(obj => text(obj, "id"))
          val healthyRow = healthyId.flatMap(id => granted.subscriptions.find(_.id == id))
          val failingRow = failingId.flatMap(id => granted.subscriptions.find(_.id == id))
          assertTrue(
            healthyRow.exists(_.refreshBefore == Start.plus(30.minutes)),
            healthyRow.exists(_.lastDeliveryAt.isDefined),
            failingRow.exists(_.refreshBefore == Start.plusSeconds(60)),
            failingRow.exists(_.lastDeliveryAt.isEmpty),
            failingId.exists(id => !left.subscriptions.exists(_.id == id)),
            healthyId.exists(id => left.subscriptions.exists(_.id == id)),
            posted.length == 1,
            posted.headOption.exists(call => call.header("X-MCP-Subscription-Id") == healthyId),
            posted.headOption.exists(_.body.contains("m2")),
          )
      }
    },
    test("an unknown event in the snapshot fails the layer") {
      ZIO.scoped {
        for
          dir <- scratch
          path = dir.resolve("subscriptions.json")
          mcp <- ZIO.fromEither(server(policy)).mapError(_.mkString(","))
          _   <- TestClock.setTime(Start)
          row = StoredSubscription.trusted(
            "sub_missing",
            "grok",
            Url,
            "missing.event",
            args,
            Start.plusSeconds(3600),
            SecretBytes,
            DeliveryHeaders.empty,
            None,
          )
          _ <- ZIO.attempt(
            Files.writeString(
              path,
              EventSnapshot.encode(EventSnapshot(Vector(row), Vector.empty)),
              StandardCharsets.UTF_8,
            )
          )
          opened <- run(mcp, path)(ZIO.service[Events]).either
        yield assertTrue(opened == Left(StoreError.Restore("unknown event")))
      }
    },
  )
end SubscriptionRestoreSpec
