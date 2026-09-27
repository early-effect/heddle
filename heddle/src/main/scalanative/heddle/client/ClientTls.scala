package heddle.client

import zio.*

/** Clients with non-default TLS trust. JVM and Native; on Node, `fetch` trusts `NODE_EXTRA_CA_CERTS`. */
object ClientTls:
  /** A pooled client that trusts only the certificates in `pem` (a private CA, or a test's self-signed cert). */
  def trusting(pem: String): ZLayer[Client.Config, ClientError, Client] =
    ClientPlatform.trusting(Some(pem))
