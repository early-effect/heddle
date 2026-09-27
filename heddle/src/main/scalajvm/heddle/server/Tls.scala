package heddle.server

import heddle.error.{HttpError, TlsError}
import heddle.internal.duplex.{ByteConn, ChannelConn, SslConn}
import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.file.{Files, Path}
import java.security.{KeyFactory, KeyStore}
import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLSocket}
import heddle.internal.Pem
import heddle.internal.engine.{ConnBuf, ConnBufPlatform}
import zio.*

final class Tls private (ctx: SSLContext):
  private[heddle] def server(conn: ByteConn, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    conn match
      case c: ChannelConn => wrap(c.ch, alpn)
      case _              =>
        ZIO.fail(HttpError.Io(IllegalArgumentException("TLS server needs a channel connection")))

  private[heddle] def wrap(ch: SocketChannel, alpn: Chunk[String]): IO[HttpError, Tls.Session] =
    val sock = ch.socket()
    ZIO
      .attempt(ctx.getSocketFactory.createSocket(sock, sock.getInetAddress.getHostName, sock.getPort, true))
      .flatMap {
        case ssl: SSLSocket =>
          ZIO
            .attemptBlocking {
              ssl.setUseClientMode(false)
              if alpn.nonEmpty then
                val params = ssl.getSSLParameters
                params.setApplicationProtocols(alpn.toArray)
                ssl.setSSLParameters(params)
              ssl.startHandshake()
              Tls.Session(ssl)
            }
            .onError(_ => ZIO.attempt(ssl.close()).ignore)
        case other =>
          ZIO.attempt(other.close()).ignore *>
            ZIO.fail(IllegalStateException(s"an SSLSocketFactory made a ${other.getClass.getName}"))
      }
      .mapError(HttpError.Io(_))
  end wrap
end Tls

object Tls:
  private[heddle] final class Session(val socket: SSLSocket):
    val conn: ByteConn = SslConn(socket)

    def applicationProtocol: String =
      Option(socket.getApplicationProtocol).getOrElse("")

    def src(buf: ByteBuffer): ConnBuf =
      ConnBufPlatform.inputStream(buf, socket.getInputStream, socket)

    def send: Chunk[Byte] => Task[Unit] =
      Tls.writer(socket.getOutputStream)
  end Session

  def layer(cert: Path, key: Path): ZLayer[Any, TlsError, Tls] =
    ZLayer.fromZIO(
      ZIO
        .attempt(Files.readString(cert) -> Files.readString(key))
        .mapError(TlsError.Unusable(_))
        .flatMap((c, k) => load(c, k))
    )

  def pem(certPem: String, keyPem: String): ZLayer[Any, TlsError, Tls] =
    ZLayer.fromZIO(load(certPem, keyPem))

  private def load(certPem: String, keyPem: String): IO[TlsError, Tls] =
    for
      certDer <- ZIO.fromEither(Pem.body(certPem, "CERTIFICATE"))
      keyDer  <- ZIO.fromEither(Pem.body(keyPem, "PRIVATE KEY"))
      tls     <- ZIO
        .attempt {
          val cert = CertificateFactory.getInstance("X.509").generateCertificate(ByteArrayInputStream(certDer))
          val key  = KeyFactory.getInstance("RSA").generatePrivate(PKCS8EncodedKeySpec(keyDer))
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
        .mapError(TlsError.Unusable(_))
    yield tls

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
