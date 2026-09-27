package heddle.client

import heddle.client.internal.{Connector, Http1Client, Http1Conn, IoFailure}
import heddle.http.{Request, Response}
import heddle.internal.duplex.{ByteConn, ChannelConn, SslConn}
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
      connect(target)
        .mapError {
          case _: SocketTimeoutException => ClientError.ConnectTimeout(target.authority)
          case e: SSLException           => ClientError.Tls(target.authority, e)
          case e                         => ClientError.Connect(target.authority, e)
        }
        .map(Http1Conn(_))

    /** Closes the channel on any failure or interruption, so a refused or failed handshake never leaks a socket. */
    private def connect(target: Target): Task[ByteConn] =
      ZIO.attempt(SocketChannel.open()).flatMap { ch =>
        ZIO
          .attemptBlockingInterrupt {
            ch.socket.connect(InetSocketAddress(target.authority.host, target.authority.port), connectMillis)
          }
          .zipRight(if target.tls then handshake(ch, target) else ZIO.succeed(ChannelConn(ch)))
          .onError(_ => ZIO.attempt(ch.close()).ignore)
      }

    private def connectMillis: Int =
      if cfg.connectTimeout == Duration.Infinity || cfg.connectTimeout.toNanos <= 0L then 0
      else math.max(1L, cfg.connectTimeout.toMillis).min(Int.MaxValue.toLong).toInt

    private def handshake(ch: SocketChannel, target: Target): Task[ByteConn] =
      val host = target.authority.host
      ZIO.attempt(ssl.getSocketFactory.createSocket(ch.socket(), host, target.authority.port, true)).flatMap {
        case sock: SSLSocket =>
          ZIO.attemptBlockingInterrupt {
            sock.setUseClientMode(true)
            val params = sock.getSSLParameters
            params.setEndpointIdentificationAlgorithm("HTTPS")
            if !isIpLiteral(host) then params.setServerNames(java.util.List.of(SNIHostName(host)))
            sock.setSSLParameters(params)
            sock.startHandshake()
            SslConn(sock)
          }
        case other =>
          ZIO.attempt(other.close()).ignore *>
            ZIO.fail(IllegalStateException(s"an SSLSocketFactory made a ${other.getClass.getName}"))
      }
    end handshake
  end JvmConnector

  private def isIpLiteral(host: String): Boolean =
    host.contains(':') || host.forall(c => c.isDigit || c == '.')
end ClientPlatform
