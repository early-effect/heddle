package heddle.mcp.events

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** One signed POST. `body` is the exact UTF-8 the signature covers. */
final case class WebhookCall(url: String, headers: Chunk[(String, String)], body: String):
  def header(name: String): Option[String] =
    headers.collectFirst { case (key, value) if key.equalsIgnoreCase(name) => value }

/** The status and body a callback returned. The body is only for the verification echo. */
final case class DeliveryReply(status: Int, body: String)

/** A typed failure of one POST. Categories a client can see are chosen from these, never from an exception message. */
enum DeliveryFault:
  case Rejected
  case ConnectionRefused
  case TimedOut
  case Tls
  case Unavailable

/** HTTPS delivery of a webhook. Tests stub it. The live sender is JVM-only; other platforms fail with `Unavailable`. */
trait WebhookTransport:
  def post(call: WebhookCall): IO[DeliveryFault, DeliveryReply]

object WebhookTransport:
  /** The platform sender. On JS and Native this is `unavailable`. */
  def live: ULayer[WebhookTransport] = WebhookTransportPlatform.live

  val unavailable: ULayer[WebhookTransport] =
    ZLayer.succeed(new WebhookTransport:
      def post(call: WebhookCall): IO[DeliveryFault, DeliveryReply] = ZIO.fail(DeliveryFault.Unavailable))
end WebhookTransport
