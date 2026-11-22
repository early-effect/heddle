package heddle.mcp.events

import heddle.*
import heddle.mcp.Mcp
import heddle.mcp.protocol.{
  ClientRequest,
  DeliveryHeaders,
  EventName,
  ExtensionId,
  ProtocolVersion,
  RequestMeta,
  RpcError,
  TtlRequest,
}
import heddle.mcp.transport.Http
import slayer.*
import zio.*
import zio.durationInt
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.time.Instant
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

final case class Filter(kind: String) derives Schema, JsonCodec
final case class Milestone(kind: String, note: String) derives Schema, JsonCodec
final case class Pair(a: Int, b: Int) derives Schema, JsonCodec

/** Webhook delivery, then poll, against a stubbed sender and `TestClock`. Push is refused. */
object EventsSpec extends ZIOSpecDefault:
  private val Url         = "https://hooks.example/events"
  private val SecretA     = whsec(3)
  private val SecretB     = whsec(9)
  private inline val Name = "session.milestone"

  private enum Unexpected:
    case Build(errors: NonEmptyChunk[heddle.mcp.McpBuildError])
    case Publish(error: EmitError)
    case Id(error: EventIdError)
    case Headers(message: String)

  private def whsec(fill: Byte): String =
    "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(fill))

  private def args(kind: String): Json.Obj = Json.Obj("kind" -> Json.Str(kind))

  private def server(
      policy: TtlPolicy = TtlPolicy.default,
      replay: Replay = Replay.default,
      modes: NonEmptyChunk[Delivery] = NonEmptyChunk(Delivery.Poll, Delivery.Webhook),
      who: Option[Principal] = Some(Principal("grok")),
      verification: VerificationPolicy = VerificationPolicy.Required,
  ): Either[NonEmptyChunk[heddle.mcp.McpBuildError], Mcp[Events.Env]] =
    Mcp
      .from(Api("Shop", "1.0.0"))
      .flatMap {
        _.event[Filter, Milestone](Name, "A session milestone").using(modes, replay)((filter, milestone) =>
          filter.kind == milestone.kind
        )
      }
      .map(_.principal(_ => ZIO.succeed(who)).eventPolicy(policy).verificationPolicy(verification))

  private def pairServer: Either[NonEmptyChunk[heddle.mcp.McpBuildError], Mcp[Events.Env]] =
    Mcp
      .from(Api("Shop", "1.0.0"))
      .flatMap {
        _.event[Pair, Pair]("pair.changed", "A pair").webhook(Replay.default)((_, _) => true)
      }
      .map(_.principal(_ => ZIO.succeed(Some(Principal("grok")))))

  private def run[E, A](mcp: Mcp[Events.Env])(
      program: ZIO[Events.Env & Stubbed[WebhookTransport], E, A]
  ): ZIO[Any, E, A] =
    program.provide(mcp.eventsLayer ++ stubbed[WebhookTransport])

  private def echoing(calls: Ref[Chunk[WebhookCall]])(
      onEvent: (WebhookCall, Int) => DeliveryReply
  ): URIO[Stubbed[WebhookTransport], Unit] =
    stub[WebhookTransport](_.post(any[WebhookCall])) { (call: WebhookCall) =>
      calls.modify { seen =>
        val next    = seen :+ call
        val ordinal = next.count(call => !verification(call))
        val reply   = if verification(call) then echo(call) else onEvent(call, ordinal)
        (reply, next)
      }
    }

  private def verification(call: WebhookCall): Boolean =
    call.body.fromJson[Json].toOption match
      case Some(obj: Json.Obj) => obj.get("type").contains(Json.Str("verification"))
      case _                   => false

  private def echo(call: WebhookCall): DeliveryReply =
    call.body.fromJson[Json].toOption match
      case Some(obj: Json.Obj) =>
        obj.get("challenge") match
          case Some(Json.Str(nonce)) => DeliveryReply(200, nonce)
          case _                     => DeliveryReply(400, "")
      case _ => DeliveryReply(400, "")

  private def subscribe(
      who: Option[Principal],
      arguments: Json.Obj = args("edit"),
      url: String = Url,
      secret: String = SecretA,
      cursor: Option[String] = None,
      ttl: TtlRequest = TtlRequest.Absent,
      maxAgeMs: Option[Long] = None,
      headers: DeliveryHeaders = DeliveryHeaders.empty,
  ): URIO[Events.Env, Either[RpcError, Json.Obj]] =
    ZIO.serviceWithZIO[Events](
      _.subscribe(
        who,
        ClientRequest.SubscribeEvents(EventName(Name), arguments, url, secret, cursor, maxAgeMs, ttl, headers),
      )
    )

  private def unsubscribe(
      who: Option[Principal],
      arguments: Json.Obj = args("edit"),
      url: String = Url,
  ): URIO[Events.Env, Either[RpcError, Json.Obj]] =
    ZIO.serviceWithZIO[Events](
      _.unsubscribe(who, ClientRequest.UnsubscribeEvents(EventName(Name), arguments, url))
    )

  private def poll(
      arguments: Json.Obj = args("edit"),
      cursor: Option[String] = None,
      maxAgeMs: Option[Long] = None,
      maxEvents: Option[Int] = None,
      who: Option[Principal] = None,
  ): URIO[Events.Env, Either[RpcError, Json.Obj]] =
    ZIO.serviceWithZIO[Events](
      _.poll(who, ClientRequest.PollEvents(EventName(Name), arguments, cursor, maxAgeMs, maxEvents))
    )

  private def emit(id: String, kind: String): ZIO[Events.Env, Unexpected, Unit] =
    for
      at   <- Clock.instant
      name <- ZIO.fromEither(EventId.from(id)).mapError(Unexpected.Id(_))
      _    <- Events.publish(EventName(Name), name, Milestone(kind, id), at).mapError(Unexpected.Publish(_))
    yield ()

  private def text(obj: Json.Obj, key: String): Option[String] =
    obj.get(key).collect { case Json.Str(value) => value }

  private def occurrences(calls: Chunk[WebhookCall]): Chunk[WebhookCall] =
    calls.filter(!verification(_))

  private def mac(secret: Chunk[Byte], id: String, timestamp: String, body: String): String =
    val hmac = Mac.getInstance("HmacSHA256")
    hmac.init(new SecretKeySpec(secret.toArray, "HmacSHA256"))
    val raw = hmac.doFinal(s"$id.$timestamp.$body".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    "v1," + Base64.getEncoder.encodeToString(raw)

  private def signedBy(call: WebhookCall, secret: WebhookSecret): Boolean =
    (call.header("webhook-id"), call.header("webhook-timestamp"), call.header("webhook-signature")) match
      case (Some(id), Some(timestamp), Some(header)) =>
        header.split(' ').exists(_ == mac(secret.bytes, id, timestamp, call.body))
      case _ => false

  def spec = suite("MCP events")(
    suite("signer and addresses")(
      test("hmac matches RFC 4231 case 1 and javax.crypto") {
        val key  = Chunk.fill(20)(0x0b.toByte)
        val data = Digests.utf8("Hi There")
        val got  = Digests.hex(Digests.hmacSha256(key, data))
        val mac  = Mac.getInstance("HmacSHA256")
        mac.init(new SecretKeySpec(key.toArray, "HmacSHA256"))
        val via = Digests.hex(Chunk.fromArray(mac.doFinal(data.toArray)))
        assertTrue(got == "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7", got == via)
      },
      test("a global address is accepted and a special-purpose address is not") {
        val rejected = List(
          "0.0.0.0",
          "10.1.2.3",
          "100.64.0.1",
          "127.0.0.1",
          "169.254.1.1",
          "172.16.0.1",
          "192.0.0.1",
          "192.0.2.1",
          "192.168.1.1",
          "198.18.0.1",
          "198.51.100.1",
          "203.0.113.1",
          "224.0.0.1",
          "240.0.0.1",
          "255.255.255.255",
          "::",
          "::1",
          "64:ff9b::1",
          "100::",
          "2001:db8::1",
          "2001:2::1",
          "fc00::1",
          "fe80::1",
          "ff02::1",
          "::ffff:127.0.0.1",
        )
        val allowed =
          List("8.8.8.8", "1.1.1.1", "172.15.0.1", "172.32.0.1", "2001:4860:4860::8888", "::2", "::ffff:8.8.8.8")
        def routable(host: String): Option[Boolean] =
          AddressPolicy.parseLiteral(host).map(AddressPolicy.globallyRoutable)
        assertTrue(
          rejected.forall(host => routable(host).contains(false)),
          allowed.forall(host => routable(host).contains(true)),
        )
      },
      test("an empty event id cannot be built") {
        assertTrue(EventId.from("").isLeft)
      },
    ),
    suite("webhook")(
      test("subscribe requires a principal, https, and a whsec secret") {
        for
          mcp    <- ZIO.fromEither(server(who = None)).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              bare  <- subscribe(None)
              http  <- subscribe(Some(Principal("grok")), url = "http://hooks.example/events")
              local <- subscribe(Some(Principal("grok")), url = "https://127.0.0.1/hook")
              bad   <- subscribe(Some(Principal("grok")), secret = "secret")
              seen  <- calls.get
            yield assertTrue(
              bare == Left(RpcError.Forbidden(None)),
              http == Left(RpcError.InvalidParams("callback url must be https")),
              local == Left(RpcError.InvalidParams("callback address is not globally routable")),
              bad == Left(
                RpcError.InvalidParams("callback secret must be whsec_ and standard base64 of 24 to 64 bytes")
              ),
              seen.isEmpty,
            )
          }
        yield result
      },
      test("an unknown event, bad arguments, and a failed challenge are typed errors") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- stub[WebhookTransport](_.post(any[WebhookCall]))((call: WebhookCall) =>
                calls.update(_ :+ call).as(DeliveryReply(200, "nope"))
              )
              missing <- ZIO.serviceWithZIO[Events](
                _.subscribe(
                  Some(Principal("grok")),
                  ClientRequest.SubscribeEvents(
                    EventName("no.such"),
                    args("edit"),
                    Url,
                    SecretA,
                    None,
                    None,
                    TtlRequest.Absent,
                    DeliveryHeaders.empty,
                  ),
                )
              )
              badArgs <- subscribe(Some(Principal("grok")), arguments = Json.Obj("nope" -> Json.Str("x")))
              failed  <- subscribe(Some(Principal("grok")))
            yield assertTrue(
              missing == Left(RpcError.NotFound(Some("event"))),
              badArgs == Left(RpcError.InvalidParams("arguments do not match the event input")),
              failed == Left(RpcError.CallbackEndpointError(heddle.mcp.protocol.CallbackFailure.ChallengeFailed)),
            )
          }
        yield result
      },
      test("the challenge is signed, cached per principal and url, and the occurrence is signed") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              first <- subscribe(Some(Principal("grok")))
              again <- subscribe(Some(Principal("grok")))
              _     <- emit("e1", "edit")
              seen  <- calls.get
              other <- subscribe(Some(Principal("other")), url = Url)
            yield
              val posts     = occurrences(seen)
              val challenge = seen.find(verification)
              val eventPost = posts.find(_.body.contains("\"eventId\":\"e1\""))
              val secret    = WebhookSecret.from(SecretA).toOption
              assertTrue(
                first.exists(obj => text(obj, "id").exists(_.startsWith("sub_")) && !obj.contains("deliveryStatus")),
                again.exists(_.contains("deliveryStatus")),
                first.toOption.flatMap(text(_, "id")) == again.toOption.flatMap(text(_, "id")),
                challenge.exists(call => secret.exists(signedBy(call, _))),
                challenge.exists(_.header("X-MCP-Subscription-Id").exists(_.startsWith("sub_"))),
                seen.count(verification) == 1,
                eventPost.exists(call => secret.exists(signedBy(call, _))),
                eventPost.exists(_.header("webhook-id").exists(id => !id.contains('.') && !id.contains("e1"))),
                other.isRight,
              )
          }
        yield result
      },
      test("caller headers ride every post, and a refresh replaces them without a new subscription") {
        val firstToken  = "Bearer receiver-one"
        val secondToken = "Bearer receiver-two"
        for
          first <- ZIO
            .fromEither(DeliveryHeaders.parse(Json.Obj("Authorization" -> Json.Str(firstToken))))
            .mapError(error => Unexpected.Headers(error.text))
          second <- ZIO
            .fromEither(DeliveryHeaders.parse(Json.Obj("Authorization" -> Json.Str(secondToken))))
            .mapError(error => Unexpected.Headers(error.text))
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              opened <- subscribe(Some(Principal("grok")), headers = first)
              _      <- emit("e1", "edit")
              mid    <- calls.get
              again  <- subscribe(Some(Principal("grok")), headers = second)
              _      <- emit("e2", "edit")
              seen   <- calls.get
            yield
              val challenge   = mid.find(verification)
              val firstEvent  = mid.find(_.body.contains("\"eventId\":\"e1\""))
              val secondEvent = seen.find(_.body.contains("\"eventId\":\"e2\""))
              val printed     =
                opened.fold(_.text, _.toJson) + again.fold(_.text, _.toJson) + seen.map(_.toString).mkString
              assertTrue(
                opened.toOption.flatMap(text(_, "id")) == again.toOption.flatMap(text(_, "id")),
                challenge.exists(_.header("Authorization").contains(firstToken)),
                firstEvent.exists(_.header("Authorization").contains(firstToken)),
                secondEvent.exists(_.header("Authorization").contains(secondToken)),
                seen.count(verification) == 1,
                !printed.contains("receiver-one"),
                !printed.contains("receiver-two"),
              )
          }
        yield result
      },
      test("skip verification commits the first subscribe without a challenge") {
        val token = "Bearer receiver-key"
        for
          headers <- ZIO
            .fromEither(DeliveryHeaders.parse(Json.Obj("Authorization" -> Json.Str(token))))
            .mapError(error => Unexpected.Headers(error.text))
          mcp    <- ZIO.fromEither(server(verification = VerificationPolicy.Skip)).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, "not-the-nonce"))
              opened <- subscribe(Some(Principal("grok")), headers = headers)
              before <- calls.get
              _      <- emit("e1", "edit")
              seen   <- calls.get
            yield
              val eventPost = seen.find(_.body.contains("\"eventId\":\"e1\""))
              assertTrue(
                opened.isRight,
                before.isEmpty,
                seen.length == 1,
                eventPost.exists(call => !verification(call) && call.header("Authorization").contains(token)),
                !opened.fold(_.text, _.toJson).contains("receiver-key"),
                !seen.map(_.toString).mkString.contains("receiver-key"),
              )
          }
        yield result
      },
      test("the https request carries caller headers and still owns hop-by-hop fields") {
        val token = "Bearer receiver-key"
        for headers <- ZIO
            .fromEither(DeliveryHeaders.parse(Json.Obj("Authorization" -> Json.Str(token))))
            .mapError(error => Unexpected.Headers(error.text))
        yield
          val call = WebhookCall(
            "https://hooks.example/events",
            Chunk(
              "content-type"      -> "application/json",
              "webhook-id"        -> "msg_1",
              "webhook-timestamp" -> "1",
              "webhook-signature" -> "v1,abc",
            ),
            "{}",
            headers,
          )
          val bytes = WebhookTransportPlatform.requestBytes(new java.net.URI(call.url), "hooks.example", 443, call)
          val lines = new String(bytes, java.nio.charset.StandardCharsets.UTF_8).split("\r\n").toList
          assertTrue(
            lines.exists(_ == s"Authorization: $token"),
            lines.count(_.toLowerCase.startsWith("host:")) == 1,
            lines.exists(_ == "Host: hooks.example"),
            lines.exists(_.startsWith("Content-Length:")),
            lines.exists(_ == "Connection: close"),
            !call.toString.contains("receiver-key"),
          )
      },
      test("two principals on one url get different subscriptions") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              a     <- subscribe(Some(Principal("a")))
              b     <- subscribe(Some(Principal("b")))
              _     <- unsubscribe(Some(Principal("a")))
              _     <- emit("e1", "edit")
              seen  <- calls.get
            yield
              val ids = occurrences(seen).flatMap(_.header("X-MCP-Subscription-Id"))
              assertTrue(
                a.toOption.flatMap(text(_, "id")) != b.toOption.flatMap(text(_, "id")),
                ids == Chunk(b.toOption.flatMap(text(_, "id")).getOrElse("missing")),
              )
          }
        yield result
      },
      test("the same canonical arguments are one subscription") {
        for
          mcp    <- ZIO.fromEither(pairServer).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              left  <- ZIO.serviceWithZIO[Events](
                _.subscribe(
                  Some(Principal("grok")),
                  ClientRequest.SubscribeEvents(
                    EventName("pair.changed"),
                    Json.Obj("b" -> Json.Num(2), "a" -> Json.Num(1)),
                    Url,
                    SecretA,
                    None,
                    None,
                    TtlRequest.Absent,
                    DeliveryHeaders.empty,
                  ),
                )
              )
              right <- ZIO.serviceWithZIO[Events](
                _.subscribe(
                  Some(Principal("grok")),
                  ClientRequest.SubscribeEvents(
                    EventName("pair.changed"),
                    Json.Obj("a" -> Json.Num(1), "b" -> Json.Num(2)),
                    Url,
                    SecretA,
                    None,
                    None,
                    TtlRequest.Absent,
                    DeliveryHeaders.empty,
                  ),
                )
              )
              seen <- calls.get
            yield assertTrue(
              left.toOption.flatMap(text(_, "id")) == right.toOption.flatMap(text(_, "id")),
              seen.count(verification) == 1,
            )
          }
        yield result
      },
      test("a filter skips the post and still advances the cursor") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              _     <- subscribe(Some(Principal("grok")))
              _     <- emit("skip", "other")
              _     <- emit("keep", "edit")
              seen  <- calls.get
              page  <- poll(cursor = Some("0"))
            yield assertTrue(
              occurrences(seen).map(_.body).exists(_.contains("\"eventId\":\"skip\"")) == false,
              occurrences(seen).map(_.body).exists(_.contains("\"eventId\":\"keep\"")),
              page.exists(obj => text(obj, "cursor").contains("2")),
              page.exists(obj =>
                obj.get("events").exists(_.toString.contains("keep")) && !obj
                  .get("events")
                  .exists(_.toString.contains("skip"))
              ),
            )
          }
        yield result
      },
      test("ttlMs is clamped to the cap and expiry drops the subscription") {
        val policy = TtlPolicy.sanitize(TtlPolicy.default.copy(cap = 2.minutes, default = 2.minutes, floor = 1.second))
        for
          mcp    <- ZIO.fromEither(server(policy)).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              at     <- Clock.instant
              opened <- subscribe(Some(Principal("grok")), ttl = TtlRequest.Millis(60L * 60L * 1000L))
              _      <- TestClock.setTime(at.plusMillis(2L * 60L * 1000L + 1000L))
              _      <- Events.maintain
              gone   <- unsubscribe(Some(Principal("grok")))
              _      <- emit("late", "edit")
              seen   <- calls.get
            yield assertTrue(
              text(opened.toOption.getOrElse(Json.Obj()), "refreshBefore")
                .contains(at.plusMillis(2L * 60L * 1000L).toString),
              gone == Left(RpcError.NotFound(Some("subscription"))),
              occurrences(seen).isEmpty,
            )
          }
        yield result
      },
      test("floor, default, and cap each set the grant") {
        val policy = TtlPolicy.sanitize(
          TtlPolicy.default.copy(floor = 2.minutes, default = 10.minutes, cap = 30.minutes)
        )
        for
          mcp    <- ZIO.fromEither(server(policy)).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              at     <- Clock.instant
              raised <- subscribe(Some(Principal("grok")), arguments = args("floor"), ttl = TtlRequest.Millis(1))
              plain  <- subscribe(Some(Principal("grok")), arguments = args("default"))
              capped <- subscribe(
                Some(Principal("grok")),
                arguments = args("cap"),
                ttl = TtlRequest.Millis(60L * 60L * 1000L),
              )
            yield
              def grant(result: Either[RpcError, Json.Obj]): Option[String] =
                result.toOption.flatMap(text(_, "refreshBefore"))
              assertTrue(
                grant(raised).contains(at.plusMillis(2.minutes.toMillis).toString),
                grant(plain).contains(at.plusMillis(10.minutes.toMillis).toString),
                grant(capped).contains(at.plusMillis(30.minutes.toMillis).toString),
              )
          }
        yield result
      },
      test("refresh moves the grant, reports delivery, and does not rewind") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              first  <- subscribe(Some(Principal("grok")), cursor = Some("0"))
              _      <- emit("e1", "edit")
              before <- calls.get
              _      <- TestClock.adjust(30.seconds)
              second <- subscribe(Some(Principal("grok")), cursor = Some("0"), secret = SecretB)
              after  <- calls.get
            yield
              val firstUntil = first.toOption
                .flatMap(text(_, "refreshBefore"))
                .flatMap(raw => scala.util.Try(Instant.parse(raw)).toOption)
              val secondUntil = second.toOption
                .flatMap(text(_, "refreshBefore"))
                .flatMap(raw => scala.util.Try(Instant.parse(raw)).toOption)
              assertTrue(
                firstUntil.exists(instant => secondUntil.exists(_.isAfter(instant))),
                second.exists(_.contains("deliveryStatus")),
                after.length == before.length,
              )
          }
        yield result
      },
      test("a rotated secret dual-signs until the grace elapses") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(500, ""))
              at    <- Clock.instant
              _     <- subscribe(Some(Principal("grok")), secret = SecretA)
              _     <- emit("e1", "edit")
              _     <- subscribe(Some(Principal("grok")), secret = SecretB)
              _     <- TestClock.setTime(at.plusSeconds(2))
              _     <- Events.maintain
              mid   <- calls.get
              _     <- TestClock.setTime(at.plusMillis(5L * 60L * 1000L + 10L * 1000L))
              _     <- Events.maintain
              late  <- calls.get
            yield
              val old     = WebhookSecret.from(SecretA).toOption
              val renewed = WebhookSecret.from(SecretB).toOption
              val retried = occurrences(mid).drop(1).headOption
              val last    = occurrences(late).lastOption
              assertTrue(
                retried.exists(call => old.exists(signedBy(call, _)) && renewed.exists(signedBy(call, _))),
                retried.exists(call =>
                  occurrences(mid).headOption.exists(_.header("webhook-id") == call.header("webhook-id"))
                ),
                retried.exists(call =>
                  occurrences(mid).headOption.exists(_.header("webhook-timestamp") != call.header("webhook-timestamp"))
                ),
                last.exists(call => renewed.exists(signedBy(call, _)) && !old.exists(signedBy(call, _))),
              )
          }
        yield result
      },
      test("a 500 reuses webhook-id and a 410 does not remove the subscription") {
        for
          mcp   <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          retry <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, n) => if n == 1 then DeliveryReply(500, "") else DeliveryReply(200, ""))
              at    <- Clock.instant
              _     <- subscribe(Some(Principal("grok")))
              _     <- emit("e1", "edit")
              _     <- TestClock.setTime(at.plusSeconds(2))
              _     <- Events.maintain
              seen  <- calls.get
            yield
              val posts = occurrences(seen)
              assertTrue(
                posts.length == 2,
                posts.headOption
                  .flatMap(_.header("webhook-id")) == posts.drop(1).headOption.flatMap(_.header("webhook-id")),
              )
          }
          gone <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, n) => if n == 1 then DeliveryReply(410, "") else DeliveryReply(200, ""))
              _     <- subscribe(Some(Principal("grok")))
              _     <- emit("e1", "edit")
              at    <- Clock.instant
              _     <- TestClock.setTime(at.plusSeconds(30))
              _     <- Events.maintain
              _     <- emit("e2", "edit")
              seen  <- calls.get
            yield assertTrue(
              occurrences(seen).length == 2,
              occurrences(seen).map(_.body).exists(_.contains("e2")),
            )
          }
        yield retry && gone
      },
      test("the subscription cap and the challenge rate limit are resource exhausted") {
        val capped = TtlPolicy.sanitize(TtlPolicy.default.copy(maxSubscriptions = 1))
        for
          mcp    <- ZIO.fromEither(server(capped)).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              _     <- subscribe(Some(Principal("grok")), url = "https://hooks.example/one")
              full  <- subscribe(Some(Principal("grok")), url = "https://hooks.example/two")
            yield assertTrue(full == Left(RpcError.ResourceExhausted("subscriptions", Some(1))))
          }
          limited <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          rate    <- run(limited) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              opened <- ZIO.foreach(1 to 8) { n =>
                subscribe(Some(Principal("grok")), url = s"https://hooks.example/h$n")
              }
              blocked <- subscribe(Some(Principal("grok")), url = "https://hooks.example/h9")
            yield assertTrue(
              opened.forall(_.isRight),
              blocked == Left(RpcError.ResourceExhausted("verification", Some(8))),
            )
          }
        yield result && rate
      },
      test("a null ttl is the cap, and a window that drops an inflight event sends a gap") {
        val policy = TtlPolicy.sanitize(TtlPolicy.default.copy(cap = 2.minutes, default = 2.minutes, floor = 1.second))
        for
          mcp <- ZIO.fromEither(server(policy)).mapError(Unexpected.Build(_))
          ttl <- run(mcp) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              at     <- Clock.instant
              opened <- subscribe(Some(Principal("grok")), ttl = TtlRequest.Unbounded)
            yield assertTrue(
              text(opened.toOption.getOrElse(Json.Obj()), "refreshBefore")
                .contains(at.plusMillis(2L * 60L * 1000L).toString)
            )
          }
          window <- ZIO
            .fromEither(server(replay = Replay.Window(2, 10.minutes)))
            .mapError(Unexpected.Build(_))
          gap <- run(window) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(500, ""))
              _     <- subscribe(Some(Principal("grok")))
              _     <- emit("e1", "edit")
              _     <- emit("e2", "edit")
              _     <- emit("e3", "edit")
              seen  <- calls.get
            yield assertTrue(occurrences(seen).exists(_.body.contains("\"type\":\"gap\"")))
          }
        yield ttl && gap
      },
    ),
    suite("poll")(
      test("a null cursor starts from now and a later cursor reads the ring") {
        for
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              empty <- poll()
              _     <- emit("e1", "edit")
              now   <- poll()
              back  <- poll(cursor = Some("0"))
              done  <- poll(cursor = Some("1"))
            yield assertTrue(
              empty.exists(obj =>
                text(obj, "cursor").contains("0") && !obj.get("events").exists(_.toString.contains("e1"))
              ),
              now.exists(obj =>
                text(obj, "cursor").contains("1") && !obj.get("events").exists(_.toString.contains("e1"))
              ),
              back.exists(_.get("events").exists(_.toString.contains("e1"))),
              done.exists(obj => !obj.get("events").exists(_.toString.contains("e1"))),
            )
          }
        yield result
      },
      test("maxEvents 1 sets hasMore and a short window reports truncated") {
        for
          mcp    <- ZIO.fromEither(server(replay = Replay.Window(2, 10.minutes))).mapError(Unexpected.Build(_))
          result <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              _     <- emit("e1", "edit")
              _     <- emit("e2", "edit")
              _     <- emit("e3", "edit")
              one   <- poll(cursor = Some("0"), maxEvents = Some(1))
              old   <- poll(cursor = Some("0"))
            yield assertTrue(
              one.exists(obj => obj.get("hasMore").contains(Json.Bool(true)) && text(obj, "cursor").contains("2")),
              old.exists(obj =>
                obj.get("truncated").contains(Json.Bool(true)) && obj.get("events").exists(_.toString.contains("e3"))
              ),
            )
          }
        yield result
      },
      test("maxAge skips an older event and an ephemeral poll returns nothing") {
        for
          mcp  <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          aged <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              _     <- emit("e1", "edit")
              _     <- TestClock.adjust(2.minutes)
              page  <- poll(cursor = Some("0"), maxAgeMs = Some(60_000L))
            yield assertTrue(
              page.exists(obj =>
                obj.get("truncated").contains(Json.Bool(true)) && !obj.get("events").exists(_.toString.contains("e1"))
              )
            )
          }
          live <- ZIO
            .fromEither(server(modes = NonEmptyChunk(Delivery.Poll, Delivery.Webhook), replay = Replay.Ephemeral))
            .mapError(Unexpected.Build(_))
          ephemeral <- run(live) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              _     <- subscribe(Some(Principal("grok")))
              _     <- emit("e1", "edit")
              page  <- poll(cursor = Some("0"))
              seen  <- calls.get
            yield assertTrue(
              page.exists(obj =>
                obj.get("cursor").contains(Json.Null) && !obj.get("events").exists(_.toString.contains("e1"))
              ),
              occurrences(seen).exists(_.body.contains("e1")),
            )
          }
        yield aged && ephemeral
      },
    ),
    suite("http")(
      test("discover advertises the extension only when an event is declared") {
        for
          plain  <- ZIO.fromEither(Mcp.from(Api("Shop", "1.0.0"))).mapError(Unexpected.Build(_))
          bare   <- plain.handle(req("server/discover", Json.Obj()))
          mcp    <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          listed <- run(mcp) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              forced = mcp.withExtension(ExtensionId.Events, Json.Obj("listChanged" -> Json.Bool(true)))
              body <- forced
                .handle(req("server/discover", Json.Obj()))
                .someOrFail(Unexpected.Build(NonEmptyChunk(heddle.mcp.McpBuildError.BadReplay)))
              page  <- ZIO.serviceWithZIO[Events](_.list(None))
              empty <- ZIO.serviceWithZIO[Events](_.list(Some("1")))
            yield assertTrue(
              bare.map(_.toJson).getOrElse("").contains("io.modelcontextprotocol/events") == false,
              body.toJson.contains("\"io.modelcontextprotocol/events\":{}"),
              body.toJson.contains("listChanged") == false,
              page.exists(_.toJson.contains("note")),
              empty.exists(obj => obj.get("events").contains(Json.Arr()) && !obj.contains("nextCursor")),
            )
          }
        yield listed
      },
      test("Mcp-Name is required, a legacy session has no events, and push is unsupported") {
        for
          mcp  <- ZIO.fromEither(server()).mapError(Unexpected.Build(_))
          http <- run(mcp) {
            for
              calls   <- Ref.make(Chunk.empty[WebhookCall])
              _       <- echoing(calls)((_, _) => DeliveryReply(200, ""))
              missing <- mcp.routes(post("events/subscribe", subscribeBody, None))
              wrong   <- mcp.routes(post("events/subscribe", subscribeBody, Some("other")))
              stream  <- mcp.handle(
                req("events/stream", Json.Obj("name" -> Json.Str(Name), "arguments" -> args("edit")))
              )
              badMax <- mcp.handle(
                req(
                  "events/poll",
                  Json.Obj("name" -> Json.Str(Name), "arguments" -> args("edit"), "maxEvents" -> Json.Num(-1)),
                )
              )
              init    <- mcp.routes(legacyInit)
              session <- ZIO
                .fromOption(init.header(Http.SessionHeader))
                .orElseFail(Unexpected.Build(NonEmptyChunk(heddle.mcp.McpBuildError.BadReplay)))
              denied <- mcp.routes(legacyList(session))
            yield assertTrue(
              missing.status == Status.BadRequest,
              missing.body.text.exists(_.contains("-32020")),
              wrong.status == Status.BadRequest,
              stream.map(_.toJson).getOrElse("").contains("-32026"),
              stream.map(_.toJson).getOrElse("").contains("push"),
              badMax.map(_.toJson).getOrElse("").contains("-32602"),
              denied.status == Status.NotFound,
              denied.body.text.exists(_.contains("-32601")),
            )
          }
        yield http
      },
    ),
  )

  private val Protocol = ProtocolVersion.Current.value

  private def req(method: String, params: Json.Obj, id: Int = 1): Json.Obj =
    val meta = Json.Obj(
      RequestMeta.VersionKey    -> Json.Str(Protocol),
      RequestMeta.ClientCapsKey -> Json.Obj(),
    )
    val body = Json.Obj((params.fields.toList :+ ("_meta" -> meta))*)
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> body)

  private val subscribeBody: Json.Obj =
    req(
      "events/subscribe",
      Json.Obj(
        "name"      -> Json.Str(Name),
        "arguments" -> args("edit"),
        "delivery"  -> Json.Obj("mode" -> Json.Str("webhook"), "url" -> Json.Str(Url), "secret" -> Json.Str(SecretA)),
      ),
    )

  private def post(method: String, body: Json.Obj, name: Option[String]): Request =
    val base = Request
      .post("/mcp", Body.json(body.toJson))
      .withHeader(Http.ProtocolHeader, Protocol)
      .withHeader(Http.MethodHeader, method)
    name.fold(base)(value => base.withHeader(Http.NameHeader, value))

  private def legacyInit: Request =
    val body = Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> Json.Num(1),
      "method"  -> Json.Str("initialize"),
      "params"  -> Json.Obj(
        "protocolVersion" -> Json.Str(ProtocolVersion.Legacy.value),
        "capabilities"    -> Json.Obj(),
      ),
    )
    Request.post("/mcp", Body.json(body.toJson)).withHeader(Http.ProtocolHeader, ProtocolVersion.Legacy.value)

  private def legacyList(session: String): Request =
    val body = Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> Json.Num(2),
      "method"  -> Json.Str("events/list"),
      "params"  -> Json.Obj(),
    )
    Request
      .post("/mcp", Body.json(body.toJson))
      .withHeader(Http.ProtocolHeader, ProtocolVersion.Legacy.value)
      .withHeader(Http.SessionHeader, session)
end EventsSpec
