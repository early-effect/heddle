package heddle.mcp.events

import zio.ULayer

private[events] object WebhookTransportPlatform:
  val live: ULayer[WebhookTransport] = WebhookTransport.unavailable
