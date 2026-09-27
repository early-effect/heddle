package heddle.client

import heddle.client.internal.{Connector, Http1Client, Http1Conn, IoFailure}
import heddle.http.{Request, Response}
import heddle.internal.duplex.{ByteConn, ChannelConn, SslConn}
import java.io.IOException
import java.net.{InetSocketAddress, SocketTimeoutException}
import java.nio.channels.SocketChannel
import javax.net.ssl.{SSLContext, SSLException, SSLSocket, SNIHostName}
import zio.*

private[heddle] object ClientPlatform:
  def layer: ZLayer[Client.Config, ClientError, Client] =
    ZLayer.scoped(ZIO.serviceWithZIO[Client.Config](cfg => systemSsl.flatMap(ssl => pooled(cfg, ssl))))

  def withSsl(ssl: SSLContext): ZLayer[Client.Config, Nothing, Client] =
    ZLayer.scoped(ZIO.serviceWithZIO[Client.Config](pooled(_, ssl)))

  def once(req: Request, config: Client.Config): IO[ClientError, Response] =
    systemSsl.flatMap(ssl => Http1Client.once(config, JvmConnector(config, ssl), req))

  def streaming(req: Request, config: Client.Config): ZIO[Scope, ClientError, Response] =
    systemSsl.flatMap(ssl => Http1Client.streaming(config, JvmConnector(config, ssl), req))

  private def pooled(cfg: Client.Config, ssl: SSLContext): URIO[Scope, Client] =
    Http1Client.scoped(cfg, JvmConnector(cfg, ssl))

  private val systemSsl: IO[ClientError, SSLContext] =
    ZIO.attempt(SSLContext.getDefault).mapError(e => ClientError.InvalidTrust(e.getMessage))

  private object JvmIo extends IoFailure:
    def apply(authority: Authority, cause: Throwable): ClientError =
      cause match
        case _: SocketTimeoutException => ClientError.ReadTimeout(authority)
        case other                     => ClientError.Io(authority, other)

  private final class JvmConnector(cfg: Client.Config, ssl: SSLContext) extends Connector:
    val io: IoFailure = JvmIo

    def open(target: Target): IO[ClientError, Http1Conn] =
      ZIO
        .attemptBlockingInterrupt(connect(target))
        .mapError {
          case _: SocketTimeoutException => ClientError.ConnectTimeout(target.authority)
          case e: SSLException           => ClientError.Tls(target.authority, e)
          case e                         => ClientError.Connect(target.authority, e)
        }
        .map(Http1Conn(_))

    /** Blocking. Closes the channel on any failure so a refused or failed handshake never leaks a socket. */
    private def connect(target: Target): ByteConn =
      val ch = SocketChannel.open()
      try
        val ms =
          if cfg.connectTimeout == Duration.Infinity || cfg.connectTimeout.toNanos <= 0L then 0
          else math.max(1L, cfg.connectTimeout.toMillis).min(Int.MaxValue.toLong).toInt
        ch.socket.connect(InetSocketAddress(target.authority.host, target.authority.port), ms)
        if !target.tls then ChannelConn(ch)
        else
          val host = target.authority.host
          val sock = ssl.getSocketFactory
            .createSocket(ch.socket(), host, target.authority.port, true)
            .asInstanceOf[SSLSocket]
          sock.setUseClientMode(true)
          val params = sock.getSSLParameters
          params.setEndpointIdentificationAlgorithm("HTTPS")
          if !isIpLiteral(host) then params.setServerNames(java.util.List.of(SNIHostName(host)))
          sock.setSSLParameters(params)
          sock.startHandshake()
          SslConn(sock)
        end if
      catch
        case e: Throwable =>
          try ch.close()
          catch case _: IOException => ()
          throw e
      end try
    end connect
  end JvmConnector

  private def isIpLiteral(host: String): Boolean =
    host.contains(':') || host.forall(c => c.isDigit || c == '.')
end ClientPlatform
