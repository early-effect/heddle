package heddle

import heddle.client.ClientTls
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.security.KeyStore
import java.security.cert.CertificateFactory
import javax.net.ssl.{SSLContext, TrustManagerFactory}
import zio.*
import zio.test.*

object TlsSpec extends ZIOSpecDefault:
  def spec =
    suite("Tls")(
      test("HTTP/1.1 over TLS returns the handler body"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TestTls.certPem, TestTls.keyPem)) { base =>
          TlsFixture
            .get(HttpClient.Version.HTTP_1_1, s"$base/health")
            .map(res => assertTrue(res.statusCode() == 200, res.body() == "ok"))
        }
      ,
      test("ALPN h2 returns the handler body"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TestTls.certPem, TestTls.keyPem)) { base =>
          TlsFixture.get(HttpClient.Version.HTTP_2, s"$base/health").map { res =>
            assertTrue(res.statusCode() == 200, res.body() == "ok", res.version() == HttpClient.Version.HTTP_2)
          }
        }
      ,
      test("heddle Client GETs HTTPS with a custom trust store"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TestTls.certPem, TestTls.keyPem)) { base =>
          TlsFixture.ssl
            .flatMap { ctx =>
              Client
                .batched(Request.get(s"$base/health"))
                .provide(ZLayer.succeed(Client.Config.default) >>> ClientTls.context(ctx))
            }
            .map(res => assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "ok"))
        }
      ,
      test("ClientTls.trusting pins the PEM it is given"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TestTls.certPem, TestTls.keyPem)) { base =>
          Client
            .batched(Request.get(s"$base/health"))
            .provide(ZLayer.succeed(Client.Config.default) >>> ClientTls.trusting(TestTls.certPem))
            .map(res => assertTrue(res.status == Status.Ok, res.body.text.is(_.some) == "ok"))
        }
      ,
      test("the default trust store rejects a self-signed peer with Tls"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TestTls.certPem, TestTls.keyPem)) { base =>
          Client
            .get(s"$base/health")
            .either
            .map(out =>
              assertTrue(out match
                case Left(_: ClientError.Tls) => true
                case _                        => false)
            )
        }
      ,
      test("a PEM with no certificate is InvalidTrust"):
        ZIO.scoped((ZLayer.succeed(Client.Config.default) >>> ClientTls.trusting("not a pem")).build).flip.map { e =>
          assertTrue(e match
            case ClientError.InvalidTrust(_) => true
            case _                           => false)
        }
      ,
      test("requireTls forbids cleartext and allows HTTPS"):
        val routes = Routes(Method.GET / "s" -> Handler.text("sec")) @@ Middleware.requireTls
        val got    =
          for
            clear <- LiveServer(routes) { base =>
              Client.get(s"$base/s").map(res => res.status.code)
            }
            tls <- LiveServer.https(routes, Tls.pem(TestTls.certPem, TestTls.keyPem)) { base =>
              TlsFixture.get(HttpClient.Version.HTTP_1_1, s"$base/s").map(_.body())
            }
          yield assertTrue(clear == 403, tls == "sec")
        got,
    ) @@ TestAspect.sequential @@ TestAspect.timeout(15.seconds) @@ TestAspect.withLiveClock
end TlsSpec

/** The JDK's view of [[TestTls]]: a trust context that accepts only it, and an `HttpClient` over that context. */
object TlsFixture:
  val ssl: Task[SSLContext] =
    ZIO.attempt {
      val cert = CertificateFactory
        .getInstance("X.509")
        .generateCertificate(ByteArrayInputStream(TestTls.certPem.getBytes(StandardCharsets.US_ASCII)))
      val ts = KeyStore.getInstance("PKCS12")
      ts.load(null, null)
      ts.setCertificateEntry("heddle", cert)
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(ts)
      val ctx = SSLContext.getInstance("TLS")
      ctx.init(null, tmf.getTrustManagers, null)
      ctx
    }

  def get(version: HttpClient.Version, url: String): Task[HttpResponse[String]] =
    ssl.flatMap { ctx =>
      ZIO.attemptBlocking(
        HttpClient
          .newBuilder()
          .sslContext(ctx)
          .version(version)
          .build()
          .send(HttpRequest.newBuilder(URI.create(url)).GET().build(), HttpResponse.BodyHandlers.ofString())
      )
    }
end TlsFixture
