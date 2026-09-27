package heddle.client

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.security.KeyStore
import java.security.cert.CertificateFactory
import javax.net.ssl.{SSLContext, TrustManagerFactory}
import scala.jdk.CollectionConverters.*
import zio.*

/** Clients with non-default TLS trust. JVM and Native; on Node, `fetch` trusts `NODE_EXTRA_CA_CERTS`. */
object ClientTls:
  /** A pooled client that trusts only the certificates in `pem` (a private CA, or a test's self-signed cert). */
  def trusting(pem: String): ZLayer[Client.Config, ClientError, Client] =
    ZLayer.fromZIO(ZIO.attempt(fromPem(pem)).mapError(e => ClientError.InvalidTrust(e.getMessage))).flatMap { env =>
      ClientPlatform.withSsl(env.get)
    }

  /** A pooled client over a caller-built `SSLContext`. */
  def context(ssl: SSLContext): ZLayer[Client.Config, Nothing, Client] =
    ClientPlatform.withSsl(ssl)

  private def fromPem(pem: String): SSLContext =
    val certs = CertificateFactory
      .getInstance("X.509")
      .generateCertificates(ByteArrayInputStream(pem.getBytes(StandardCharsets.US_ASCII)))
      .asScala
      .toList
    if certs.isEmpty then throw IllegalArgumentException("trust PEM holds no certificates")
    val store = KeyStore.getInstance(KeyStore.getDefaultType)
    store.load(null, null)
    certs.zipWithIndex.foreach((c, i) => store.setCertificateEntry(s"trusted-$i", c))
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(store)
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(null, tmf.getTrustManagers, null)
    ctx
  end fromPem
end ClientTls
