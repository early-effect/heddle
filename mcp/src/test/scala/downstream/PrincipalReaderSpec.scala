package downstream

import heddle.endpoint.{Api, Schema}
import heddle.http.header.Headers
import heddle.mcp.{Mcp, McpBuildError}
import heddle.mcp.events.{
  EmitError,
  EventId,
  Events,
  Principal,
  Replay,
  TtlPolicy,
  VerificationPolicy,
  WebhookTransport,
}
import heddle.mcp.protocol.{EventName, ProtocolVersion, RequestMeta, RpcError}
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{NonEmptyChunk, ZIO}

import java.time.Instant
import java.util.Base64

/** A caller outside `heddle.mcp`. Subscribe refuses a missing principal. Poll reads the ring on `TestClock`. */
object PrincipalReaderSpec extends ZIOSpecDefault:
  private val When        = Instant.parse("2026-11-21T15:04:05Z")
  private inline val Name = "note.added"
  private val Header      = "X-Principal"
  private val Url         = "https://hooks.example/events"
  private val Secret      = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(3.toByte))
  private val Grant       = When.plusMillis(TtlPolicy.default.default.toMillis).toString
  private val Aged        = When.plusMillis(Replay.DefaultAge.toMillis + 1L)

  private enum Failed:
    case Build(errors: NonEmptyChunk[McpBuildError])
    case Publish(error: EmitError)

  final case class Note(text: String) derives Schema, JsonCodec

  private val arguments: Json.Obj = Json.Obj("text" -> Json.Str("hello"))

  def spec = suite("principal reader")(
    test("subscribe without a principal is forbidden and poll returns the event") {
      ZIO.fromEither(server).mapError(Failed.Build(_)).flatMap { mcp =>
        val body =
          for
            _       <- TestClock.setTime(When)
            missing <- mcp.handle(subscribe)
            granted <- mcp.handle(subscribe, Headers.empty.add(Header, "ada"))
            _       <- Events
              .publish(EventName(Name), EventId("n1"), Note("hello"), When)
              .mapError(Failed.Publish(_))
            page <- mcp.handle(poll, Headers.empty.add(Header, "ada"))
            _    <- TestClock.setTime(Aged)
            aged <- mcp.handle(poll, Headers.empty.add(Header, "ada"))
          yield assertTrue(
            forbidden(missing),
            refresh(granted).contains(Grant),
            result(page).exists(hasNote),
            result(aged).exists(body => truncated(body) && !hasNote(body)),
          )
        body.provide(mcp.eventsLayer ++ WebhookTransport.unavailable)
      }
    }
  )

  private def server: Either[NonEmptyChunk[McpBuildError], Mcp[Events.Env]] =
    Mcp
      .from(Api("Shop", "1"))
      .flatMap {
        _.event[Note, Note](Name, "A note").pollAndWebhook((_, _) => true)
      }
      .map { declared =>
        declared
          .principal(headers => ZIO.succeed(headers.get(Header).map(Principal(_))))
          .verificationPolicy(VerificationPolicy.Skip)
      }

  private def request(method: String, params: Json.Obj): Json.Obj =
    val meta = Json.Obj(
      RequestMeta.VersionKey    -> Json.Str(ProtocolVersion.Current.value),
      RequestMeta.ClientCapsKey -> Json.Obj(),
    )
    val body = Json.Obj((params.fields.toList :+ ("_meta" -> meta))*)
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(1), "method" -> Json.Str(method), "params" -> body)

  private val subscribe: Json.Obj =
    request(
      "events/subscribe",
      Json.Obj(
        "name"      -> Json.Str(Name),
        "arguments" -> arguments,
        "delivery"  -> Json.Obj("mode" -> Json.Str("webhook"), "url" -> Json.Str(Url), "secret" -> Json.Str(Secret)),
      ),
    )

  private val poll: Json.Obj =
    request(
      "events/poll",
      Json.Obj("name" -> Json.Str(Name), "arguments" -> arguments, "cursor" -> Json.Str("0")),
    )

  private def forbidden(message: Option[Json]): Boolean =
    val expected = BigDecimal(RpcError.Forbidden(None).code)
    code(message).exists(_.compareTo(expected) == 0)

  private def code(message: Option[Json]): Option[BigDecimal] =
    message match
      case Some(obj: Json.Obj) =>
        obj.get("error") match
          case Some(err: Json.Obj) =>
            err.get("code") match
              case Some(Json.Num(found)) => Some(found)
              case _                     => None
          case _ => None
      case _ => None

  private def refresh(message: Option[Json]): Option[String] =
    result(message).flatMap { body =>
      body.get("refreshBefore") match
        case Some(Json.Str(value)) => Some(value)
        case _                     => None
    }

  private def result(message: Option[Json]): Option[Json.Obj] =
    message match
      case Some(obj: Json.Obj) =>
        obj.get("result") match
          case Some(body: Json.Obj) => Some(body)
          case _                    => None
      case _ => None

  private def hasNote(body: Json.Obj): Boolean =
    body.get("events") match
      case Some(Json.Arr(rows)) =>
        rows.exists {
          case row: Json.Obj =>
            row.get("data").exists(_.as[Note].contains(Note("hello"))) &&
            row.get("timestamp").contains(Json.Str(When.toString))
          case _ => false
        }
      case _ => false

  private def truncated(body: Json.Obj): Boolean =
    body.get("truncated").contains(Json.Bool(true))
end PrincipalReaderSpec
