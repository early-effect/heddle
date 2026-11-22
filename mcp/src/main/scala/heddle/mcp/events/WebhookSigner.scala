package heddle.mcp.events

import zio.Chunk

import java.util.Base64

/** Standard Webhooks `v1,` HMAC. Several secrets produce a space-separated header, current first. */
private[events] object WebhookSigner:
  def header(webhookId: String, timestampSeconds: Long, body: String, secrets: Chunk[Chunk[Byte]]): String =
    val material = Digests.utf8(s"$webhookId.$timestampSeconds.$body")
    secrets
      .map { secret =>
        val mac = Digests.hmacSha256(secret, material)
        "v1," + Base64.getEncoder.encodeToString(mac.toArray)
      }
      .mkString(" ")
