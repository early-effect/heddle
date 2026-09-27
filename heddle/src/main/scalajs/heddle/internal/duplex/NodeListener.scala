package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.node.{boundPort, Net, NetServer, NetSocket, NodeTls, TlsOptions, TlsSocket}
import scala.collection.mutable
import scala.scalajs.js
import zio.*

/** A Node server's connections as a pull: sockets that arrive before `accept` wait in `pending`, and `accept` calls
  * that arrive before a socket wait in `waiters`. Node runs this on one thread, so the queues need no locks.
  */
private[heddle] final class NodeListener[S <: js.Object](server: NetServer, event: String, wrap: S => ByteConn)
    extends Listener:
  private val pending                         = mutable.Queue.empty[S]
  private val waiters                         = mutable.Queue.empty[IO[AcceptError, ByteConn] => Unit]
  private var closed                          = false
  private var closeWaiter: Option[() => Unit] = None

  server.on(
    event,
    (
        (sock: S) =>
          if waiters.nonEmpty then waiters.dequeue()(ZIO.succeed(wrap(sock)))
          else pending.enqueue(sock)
    ): js.Function1[S, Unit],
  )
  server.on(
    "close",
    (() =>
      closed = true
      while waiters.nonEmpty do waiters.dequeue()(ZIO.fail(AcceptError.Closed))
      closeWaiter.foreach(_())
      closeWaiter = None
    ): js.Function0[Unit],
  )

  def localPort: UIO[Int] = ZIO.succeed(server.boundPort)

  def accept: IO[AcceptError, ByteConn] =
    ZIO.suspendSucceed {
      if pending.nonEmpty then ZIO.succeed(wrap(pending.dequeue()))
      else if closed then ZIO.fail(AcceptError.Closed)
      else
        ZIO.async[Any, AcceptError, ByteConn] { cb =>
          if pending.nonEmpty then cb(ZIO.succeed(wrap(pending.dequeue())))
          else if closed then cb(ZIO.fail(AcceptError.Closed))
          else waiters.enqueue(cb)
        }
    }

  def close: UIO[Unit] =
    ZIO.async[Any, Nothing, Unit] { cb =>
      if closed || !server.listening then cb(ZIO.unit)
      else
        closeWaiter = Some(() => cb(ZIO.unit))
        val _ = server.close(() => ())
    }
end NodeListener

private[heddle] object NodeListener:
  def plain(config: Server.Config): IO[ServerError, Listener] =
    bind(config, Net.createServer(), "connection", NodeConn.of)

  def tls(config: Server.Config, certPem: String, keyPem: String): IO[ServerError, Listener] =
    val alpn = if config.http2 then js.Array("h2", "http/1.1") else js.Array("http/1.1")
    bind(
      config,
      NodeTls.createServer(TlsOptions(key = keyPem, cert = certPem, ALPNProtocols = alpn), (_: TlsSocket) => ()),
      "secureConnection",
      NodeConn.tls,
    )

  private def bind[S <: js.Object](
      config: Server.Config,
      server: => NetServer,
      event: String,
      wrap: S => ByteConn,
  ): IO[ServerError, Listener] =
    ZIO.async[Any, ServerError, Listener] { cb =>
      val bound = server
      bound.once(
        "error",
        (
            (err: js.Error) =>
              cb(
                ZIO.fail(
                  ServerError.BindFailed(
                    config.host,
                    config.port,
                    java.io.IOException(Option(err.message).getOrElse("listen failed")),
                  )
                )
              )
        ): js.Function1[js.Error, Unit],
      )
      val _ = bound.listen(config.port, config.host, () => cb(ZIO.succeed(NodeListener(bound, event, wrap))))
    }
end NodeListener
