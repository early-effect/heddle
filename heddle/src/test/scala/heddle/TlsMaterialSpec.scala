package heddle

import heddle.server.Tls
import zio.*
import zio.test.*

object TlsMaterialSpec extends ZIOSpecDefault:
  private def load(cert: String, key: String): IO[TlsError, Tls] =
    ZIO.service[Tls].provideLayer(Tls.pem(cert, key))

  def spec = suite("TLS material")(
    test("text with no PEM block fails at startup, naming the block it looked for"):
      load("not a certificate", "not a key").flip.map(e => assertTrue(e == TlsError.MissingPem("CERTIFICATE")))
    ,
    test("a certificate with no key block names the key"):
      load("-----BEGIN CERTIFICATE-----\nAAAA\n-----END CERTIFICATE-----", "").flip.map { e =>
        assertTrue(e == TlsError.MissingPem("PRIVATE KEY"))
      },
  ) @@ TestAspect.timeout(30.seconds)
end TlsMaterialSpec
