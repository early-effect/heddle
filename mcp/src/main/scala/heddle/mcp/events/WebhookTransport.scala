package heddle.mcp.events

import heddle.mcp.protocol.DeliveryHeaders
import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

/** One signed POST. `body` is the exact UTF-8 the signature covers. `headers` are the signing headers. `delivery` is
  * the caller's extra headers. Their values are secrets: `DeliveryHeaders.toString` omits them, and they are not
  * covered by the signature.
  */
final case class WebhookCall(
    url: String,
    headers: Chunk[(String, String)],
    body: String,
    delivery: DeliveryHeaders = DeliveryHeaders.empty,
):
  def header(name: String): Option[String] =
    def find(pairs: Chunk[(String, String)]): Option[String] =
      pairs.collectFirst { case (key, value) if key.equalsIgnoreCase(name) => value }
    find(headers).orElse(find(delivery.entries))

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
