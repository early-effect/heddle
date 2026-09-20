package heddle.server

import heddle.error.HttpError
import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.{KeyFactory, KeyStore}
import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLSocket}
import heddle.internal.engine.{ConnBuf, ConnBufPlatform}
import zio.*

final class Tls private (ctx: SSLContext):
  private[heddle] def wrap(ch: SocketChannel, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    ZIO
      .attempt {
        val sock = ch.socket()
        val ssl  = ctx.getSocketFactory
          .createSocket(sock, sock.getInetAddress.getHostName, sock.getPort, true)
          .asInstanceOf[SSLSocket]
        try
          ssl.setUseClientMode(false)
          if alpn.nonEmpty then
            val params = ssl.getSSLParameters
            params.setApplicationProtocols(alpn.toArray)
            ssl.setSSLParameters(params)
          ssl.startHandshake()
          Tls.Session(ssl)
        catch
          case e: Throwable =>
            try ssl.close()
            catch case _: Throwable => ()
            throw e
        end try
      }
      .mapError(HttpError.Io(_))
end Tls

object Tls:
  private[heddle] final class Session(val socket: SSLSocket):
    def applicationProtocol: String =
      Option(socket.getApplicationProtocol).getOrElse("")

    def src(buf: ByteBuffer): ConnBuf =
      ConnBufPlatform.inputStream(buf, socket.getInputStream, socket)

    def send: Chunk[Byte] => Task[Unit] =
      Tls.writer(socket.getOutputStream)

  def layer(cert: Path, key: Path): ZLayer[Any, HttpError, Tls] =
    ZLayer.fromZIO(
      ZIO
        .attempt(Files.readString(cert) -> Files.readString(key))
        .mapError(HttpError.Io(_))
        .flatMap((c, k) => load(c, k))
    )

  def pem(certPem: String, keyPem: String): ZLayer[Any, HttpError, Tls] =
    ZLayer.fromZIO(load(certPem, keyPem))

  private def load(certPem: String, keyPem: String): IO[HttpError, Tls] =
    ZIO
      .attempt {
        val cert = CertificateFactory
          .getInstance("X.509")
          .generateCertificate(ByteArrayInputStream(pemBody(certPem, "CERTIFICATE")))
        val spec = PKCS8EncodedKeySpec(pemBody(keyPem, "PRIVATE KEY"))
        val key  = KeyFactory.getInstance("RSA").generatePrivate(spec)
        val ks   = KeyStore.getInstance("PKCS12")
        val pass = "heddle".toCharArray
        ks.load(null, pass)
        ks.setKeyEntry("heddle", key, pass, Array(cert))
        val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
        kmf.init(ks, pass)
        val ctx = SSLContext.getInstance("TLS")
        ctx.init(kmf.getKeyManagers, null, null)
        Tls(ctx)
      }
      .mapError(HttpError.Io(_))

  private def pemBody(pem: String, kind: String): Array[Byte] =
    val begin = s"-----BEGIN $kind-----"
    val end   = s"-----END $kind-----"
    val from  = pem.indexOf(begin)
    val until = pem.indexOf(end)
    if from < 0 || until < 0 then throw IllegalArgumentException(s"missing PEM $kind")
    val b64 = pem.substring(from + begin.length, until).filterNot(_.isWhitespace)
    java.util.Base64.getDecoder.decode(b64.getBytes(StandardCharsets.US_ASCII))

  private[heddle] def writer(out: OutputStream): Chunk[Byte] => Task[Unit] =
    chunk =>
      ZIO.attempt {
        if chunk.nonEmpty then
          out.write(chunk.toArray)
          out.flush()
      }

  private[heddle] def pull(in: InputStream, n: Int): IO[HttpError, Option[Chunk[Byte]]] =
    ZIO
      .attemptBlockingInterrupt {
        val arr = Array.ofDim[Byte](n)
        val got = in.read(arr)
        if got < 0 then None
        else Some(Chunk.fromArray(if got == arr.length then arr else arr.take(got)))
      }
      .mapError(HttpError.Io(_))
end Tls
