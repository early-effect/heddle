package heddle

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.KeyStore
import java.security.cert.X509Certificate
import javax.net.ssl.{SSLContext, TrustManagerFactory}
import zio.*
import zio.test.*

object TlsSpec extends ZIOSpecDefault:
  def spec =
    suite("Tls")(
      test("HTTP/1.1 over TLS returns the handler body"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TlsFixture.certPem, TlsFixture.keyPem)) { base =>
          ZIO
            .attempt {
              val res = TlsFixture
                .client(HttpClient.Version.HTTP_1_1)
                .send(
                  HttpRequest.newBuilder(URI.create(s"$base/health")).GET().build(),
                  HttpResponse.BodyHandlers.ofString(),
                )
              (res.statusCode(), res.body())
            }
            .map((code, body) => assertTrue(code == 200, body == "ok"))
        }
      ,
      test("ALPN h2 returns the handler body"):
        val routes = Routes(Method.GET / "health" -> Handler.text("ok"))
        LiveServer.https(routes, Tls.pem(TlsFixture.certPem, TlsFixture.keyPem)) { base =>
          ZIO
            .attempt {
              val res = TlsFixture
                .client(HttpClient.Version.HTTP_2)
                .send(
                  HttpRequest.newBuilder(URI.create(s"$base/health")).GET().build(),
                  HttpResponse.BodyHandlers.ofString(),
                )
              (res.statusCode(), res.body(), res.version())
            }
            .map { (code, body, ver) =>
              assertTrue(code == 200, body == "ok", ver == HttpClient.Version.HTTP_2)
            }
        }
      ,
      test("requireTls forbids cleartext and allows HTTPS"):
        val routes = Routes(Method.GET / "s" -> Handler.text("sec")) @@ Middleware.requireTls
        val got    =
          for
            clear <- LiveServer(routes) { base =>
              Client.get(s"$base/s").map(res => res.status.code)
            }
            tls <- LiveServer.https(routes, Tls.pem(TlsFixture.certPem, TlsFixture.keyPem)) { base =>
              ZIO.attempt {
                TlsFixture
                  .client(HttpClient.Version.HTTP_1_1)
                  .send(
                    HttpRequest.newBuilder(URI.create(s"$base/s")).GET().build(),
                    HttpResponse.BodyHandlers.ofString(),
                  )
                  .body()
              }
            }
          yield assertTrue(clear == 403, tls == "sec")
        got,
    ) @@ TestAspect.sequential @@ TestAspect.timeout(15.seconds) @@ TestAspect.withLiveClock
end TlsSpec

object TlsFixture:
  private lazy val generated: (String, String, SSLContext) = generate()

  def certPem: String = generated._1
  def keyPem: String  = generated._2

  def client(version: HttpClient.Version): HttpClient =
    HttpClient.newBuilder().sslContext(generated._3).version(version).build()

  private def generate(): (String, String, SSLContext) =
    val dir = java.nio.file.Files.createTempDirectory("heddle-tls")
    val p12 = dir.resolve("ks.p12")
    val pb  = ProcessBuilder(
      "keytool",
      "-genkeypair",
      "-alias",
      "heddle",
      "-keyalg",
      "RSA",
      "-keysize",
      "2048",
      "-validity",
      "2",
      "-storetype",
      "PKCS12",
      "-keystore",
      p12.toString,
      "-storepass",
      "changeit",
      "-dname",
      "CN=localhost",
      "-ext",
      "SAN=DNS:localhost,IP:127.0.0.1",
      "-noprompt",
    )
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val log  = String(proc.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    if proc.waitFor() != 0 then throw RuntimeException(s"keytool failed: $log")
    val ks   = KeyStore.getInstance("PKCS12")
    val pass = "changeit".toCharArray
    ks.load(java.nio.file.Files.newInputStream(p12), pass)
    val cert    = ks.getCertificate("heddle").asInstanceOf[X509Certificate]
    val key     = ks.getKey("heddle", pass)
    val certPem =
      "-----BEGIN CERTIFICATE-----\n" +
        java.util.Base64.getMimeEncoder(64, Array('\n'.toByte)).encodeToString(cert.getEncoded) +
        "\n-----END CERTIFICATE-----\n"
    val keyPem =
      "-----BEGIN PRIVATE KEY-----\n" +
        java.util.Base64.getMimeEncoder(64, Array('\n'.toByte)).encodeToString(key.getEncoded) +
        "\n-----END PRIVATE KEY-----\n"
    val ts = KeyStore.getInstance("PKCS12")
    ts.load(null, pass)
    ts.setCertificateEntry("heddle", cert)
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ts)
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(null, tmf.getTrustManagers, null)
    (certPem, keyPem, ctx)
  end generate
end TlsFixture
