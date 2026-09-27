package heddle.client.internal

import heddle.client.{Client, ClientError, Target}
import heddle.http.{Body, Method, Request, Response}
import zio.*

/** Opens connections for one platform. */
private[heddle] trait Connector:
  def open(target: Target): IO[ClientError, Http1Conn]
  def io: IoFailure

/** Pooled HTTP/1.1 client over a platform [[Connector]] (JVM sockets, POSIX on Native). */
private[heddle] final class Http1Client(cfg: Client.Config, pool: ConnPool, connector: Connector) extends Client:
  def batched(req: Request): IO[ClientError, Response] =
    ZIO.fromEither(Target.of(req)).flatMap { target =>
      val headers = ClientSupport.prepare(cfg, req.headers)
      pool
        .exchange(target, Http1Client.replay(req)) { c =>
          Http1Client.strict(cfg, connector.io, c, target, req.copy(headers = headers))
        }
        .flatMap(ClientSupport.inflate(cfg, target, headers))
    }

  def streaming(req: Request): ZIO[Scope, ClientError, Response] =
    Http1Client.streaming(cfg, connector, req)
end Http1Client

private[heddle] object Http1Client:
  def scoped(cfg: Client.Config, connector: Connector): URIO[Scope, Client] =
    ConnPool.scoped(cfg, connector.open).map(Http1Client(cfg, _, connector))

  /** One exchange on its own connection, closed afterwards. */
  def once(cfg: Client.Config, connector: Connector, req: Request): IO[ClientError, Response] =
    ZIO.fromEither(Target.of(req)).flatMap { target =>
      val headers = ClientSupport.prepare(cfg, req.headers)
      ZIO
        .acquireReleaseWith(connector.open(target))(_.conn.close) { c =>
          strict(cfg, connector.io, c, target, req.copy(headers = headers)).map(_._1)
        }
        .flatMap(ClientSupport.inflate(cfg, target, headers))
    }

  /** The response head, and a body that reads from a dedicated connection until `Scope` closes. */
  def streaming(cfg: Client.Config, connector: Connector, req: Request): ZIO[Scope, ClientError, Response] =
    for
      target <- ZIO.fromEither(Target.of(req))
      c      <- ZIO.acquireRelease(connector.open(target))(_.conn.close)
      _    <- Http1Exchange.send(c, target, req.method, ClientSupport.prepare(cfg, req.headers), req.body, connector.io)
      _    <- c.src.setReadTimeout(cfg.idleTimeout)
      head <- Http1Exchange.readHead(c, target, Limits.of(cfg), connector.io)
      framing <- ZIO.fromEither(Http1Exchange.framing(req.method, head)).mapError(Http1Exchange.malformed(target))
    yield Http1Exchange.response(head, Http1Exchange.streamBody(c, framing, head.headers.contentType))

  def strict(
      cfg: Client.Config,
      io: IoFailure,
      c: Http1Conn,
      target: Target,
      req: Request,
  ): IO[ClientError, (Response, Reuse)] =
    val limits = Limits.of(cfg)
    for
      _       <- Http1Exchange.send(c, target, req.method, req.headers, req.body, io)
      _       <- c.src.setReadTimeout(cfg.idleTimeout)
      head    <- Http1Exchange.readHead(c, target, limits, io)
      framing <- ZIO.fromEither(Http1Exchange.framing(req.method, head)).mapError(Http1Exchange.malformed(target))
      bytes   <- Http1Exchange.readBody(c, target, framing, limits, io)
    yield (
      Http1Exchange.response(head, Body.fromBytes(bytes, head.headers.contentType)),
      Http1Exchange.reuse(req.headers, head, framing),
    )
    end for
  end strict

  /** Idempotent methods with a body that can be sent twice (RFC 9110 §9.2.2). */
  def replay(req: Request): Replay =
    val idempotent = req.method match
      case Method.GET | Method.HEAD | Method.OPTIONS | Method.PUT | Method.DELETE | Method.TRACE => true
      case _                                                                                     => false
    req.body match
      case Body.Stream(_, _, _) => Replay.Unsafe
      case _                    => if idempotent then Replay.Safe else Replay.Unsafe
end Http1Client
