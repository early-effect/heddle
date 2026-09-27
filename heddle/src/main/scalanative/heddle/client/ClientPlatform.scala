package heddle.client

import heddle.client.internal.{Connector, Http1Client, Http1Conn, IoFailure}
import heddle.http.{Request, Response}
import heddle.internal.duplex.NativeConn
import heddle.internal.openssl.Ssl
import heddle.internal.posix.{AsyncFd, Net, NetError, SockAddr, SslIo, Syscall}
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
      ZIO.suspendSucceed(ZIO.fromEither(Ssl.clientCtx(trustPem))).mapError(e => ClientError.InvalidTrust(e.message))
    )(ctx => ZIO.succeed(ctx.close()))

  private object NativeIo extends IoFailure:
    def apply(authority: Authority, cause: Throwable): ClientError = ClientError.Io(authority, cause)

  private final class NativeConnector(cfg: Client.Config, ctx: Ssl.Ctx) extends Connector:
    val io: IoFailure = NativeIo

    def open(target: Target): IO[ClientError, Http1Conn] =
      val a         = target.authority
      val connected =
        for
          addrs <- ZIO
            .blocking(ZIO.suspendSucceed(ZIO.fromEither(Net.resolve(a.host, a.port))))
            .mapError(connectFailed(a))
          fd   <- first(a, addrs.toList)
          conn <- if target.tls then secure(target, fd) else ZIO.succeed(NativeConn.of(fd))
        yield Http1Conn(conn)
      if cfg.connectTimeout == Duration.Infinity || cfg.connectTimeout.toNanos <= 0L then connected
      else connected.timeoutFail(ClientError.ConnectTimeout(a))(cfg.connectTimeout)
    end open

    /** Tries each resolved address in order. A socket is closed unless its connect succeeds. */
    private def first(a: Authority, addrs: List[SockAddr]): IO[ClientError, Int] =
      addrs match
        case Nil          => ZIO.fail(connectFailed(a)(NetError.NoAddress(a.host)))
        case addr :: rest =>
          ZIO
            .acquireReleaseExitWith(ZIO.suspendSucceed(ZIO.fromEither(Net.connect(addr))).mapError(connectFailed(a)))(
              (fd: Int, exit: Exit[ClientError, Int]) => ZIO.succeed(Net.close(fd)).unless(exit.isSuccess).unit
            ) { fd =>
              AsyncFd.writable(fd).mapError(connectFailed(a)) *>
                ZIO.suspendSucceed(ZIO.fromEither(Net.socketError(fd))).mapError(connectFailed(a)).flatMap { err =>
                  if err == 0 then ZIO.succeed(Net.setTcpNoDelay(fd, true)).as(fd)
                  else ZIO.fail(connectFailed(a)(NetError.Failed(Syscall.Connect, err)))
                }
            }
            .catchSome { case _: ClientError.Connect if rest.nonEmpty => first(a, rest) }

    /** A failed handshake frees the session and closes the socket. */
    private def secure(target: Target, fd: Int): IO[ClientError, NativeConn] =
      val a = target.authority
      ZIO
        .acquireReleaseExitWith(
          ZIO
            .suspendSucceed(ZIO.fromEither(Ssl.connect(ctx, fd, a.host)))
            .tapError(_ => ZIO.succeed(Net.close(fd)))
            .mapError(e => ClientError.Tls(a, e.exception))
        )((session: Ssl.Session, exit: Exit[ClientError, NativeConn]) =>
          ZIO.succeed { session.close(); Net.close(fd) }.unless(exit.isSuccess).unit
        )(session =>
          SslIo
            .handshake(session, accept = false, fd)
            .mapBoth(e => ClientError.Tls(a, e.exception), _ => NativeConn.tls(fd, session))
        )
    end secure

    private def connectFailed(a: Authority)(e: NetError): ClientError = ClientError.Connect(a, e.exception)
  end NativeConnector
end ClientPlatform
