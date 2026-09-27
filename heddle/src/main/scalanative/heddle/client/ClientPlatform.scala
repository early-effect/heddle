package heddle.client

import heddle.client.internal.{Connector, Http1Client, Http1Conn, IoFailure}
import heddle.http.{Request, Response}
import heddle.internal.duplex.NativeConn
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{AsyncFd, Net, SockAddr, SslIo}
import zio.*

private[heddle] object ClientPlatform:
  def layer: ZLayer[Client.Config, ClientError, Client] =
    trusting(None)

  /** A pooled client whose TLS context verifies against `trustPem`, or the system store when `None`. */
  def trusting(trustPem: Option[String]): ZLayer[Client.Config, ClientError, Client] =
    ZLayer.scoped {
      for
        cfg    <- ZIO.service[Client.Config]
        ctx    <- context(trustPem)
        client <- Http1Client.scoped(cfg, NativeConnector(cfg, ctx))
      yield client
    }

  def once(req: Request, config: Client.Config): IO[ClientError, Response] =
    ZIO.scoped(context(None).flatMap(ctx => Http1Client.once(config, NativeConnector(config, ctx), req)))

  def streaming(req: Request, config: Client.Config): ZIO[Scope, ClientError, Response] =
    context(None).flatMap(ctx => Http1Client.streaming(config, NativeConnector(config, ctx), req))

  /** Sessions hold their own reference to the context, so freeing it at scope end is safe. */
  private def context(trustPem: Option[String]): ZIO[Scope, ClientError, Ssl.Ctx] =
    ZIO.acquireRelease(
      ZIO.attempt(Ssl.clientCtx(trustPem)).mapError(e => ClientError.InvalidTrust(e.getMessage))
    )(ctx => ZIO.succeed(ctx.close()))

  private object NativeIo extends IoFailure:
    def apply(authority: Authority, cause: Throwable): ClientError = ClientError.Io(authority, cause)

  private final class NativeConnector(cfg: Client.Config, ctx: Ssl.Ctx) extends Connector:
    val io: IoFailure = NativeIo

    def open(target: Target): IO[ClientError, Http1Conn] =
      val a         = target.authority
      val connected =
        for
          addrs <- ZIO.attemptBlocking(Net.resolve(a.host, a.port)).mapError(ClientError.Connect(a, _))
          fd    <- first(a, addrs.toList)
          conn  <- if target.tls then secure(target, fd) else ZIO.succeed(NativeConn.of(fd))
        yield Http1Conn(conn)
      if cfg.connectTimeout == Duration.Infinity || cfg.connectTimeout.toNanos <= 0L then connected
      else connected.timeoutFail(ClientError.ConnectTimeout(a))(cfg.connectTimeout)
    end open

    /** Tries each resolved address in order. A socket is closed unless its connect succeeds. */
    private def first(a: Authority, addrs: List[SockAddr]): IO[ClientError, Int] =
      addrs match
        case Nil          => ZIO.fail(ClientError.Connect(a, java.io.IOException(s"no address for ${a.host}")))
        case addr :: rest =>
          ZIO
            .acquireReleaseExitWith(ZIO.attempt(Net.connect(addr)).mapError(ClientError.Connect(a, _)))(
              (fd: Int, exit: Exit[ClientError, Int]) => ZIO.succeed(Net.close(fd)).unless(exit.isSuccess).unit
            ) { fd =>
              AsyncFd.writable(fd).mapError(ClientError.Connect(a, _)) *>
                ZIO.attempt(Net.socketError(fd)).mapError(ClientError.Connect(a, _)).flatMap { err =>
                  if err == 0 then ZIO.attempt(Net.setTcpNoDelay(fd, true)).ignore.as(fd)
                  else ZIO.fail(ClientError.Connect(a, java.io.IOException(s"connect errno $err")))
                }
            }
            .catchSome { case _: ClientError.Connect if rest.nonEmpty => first(a, rest) }

    /** A failed handshake frees the session and closes the socket. */
    private def secure(target: Target, fd: Int): IO[ClientError, NativeConn] =
      val a = target.authority
      ZIO
        .acquireReleaseExitWith(
          ZIO
            .attempt(Ssl.connect(ctx, fd, a.host))
            .tapError(_ => ZIO.succeed(Net.close(fd)))
            .mapError(ClientError.Tls(a, _))
        )((session: Ssl.Session, exit: Exit[ClientError, NativeConn]) =>
          ZIO.succeed { session.close(); Net.close(fd) }.unless(exit.isSuccess).unit
        )(session =>
          SslIo.handshake(session, accept = false, fd).mapBoth(ClientError.Tls(a, _), _ => NativeConn.tls(fd, session))
        )
    end secure
  end NativeConnector
end ClientPlatform
