package heddle.internal.duplex

import heddle.Server
import heddle.error.ServerError
import heddle.internal.node.{AddressInfo, NetServer, NodeTls, TlsOptions, TlsSocket}
import scala.collection.mutable
import scala.scalajs.js
import zio.*

private[heddle] final class TlsListener(server: NetServer) extends Listener:
  private val pending                         = mutable.Queue.empty[TlsSocket]
  private val waiters                         = mutable.Queue.empty[IO[Throwable, ByteConn] => Unit]
  private var closed                          = false
  private var closeWaiter: Option[() => Unit] = None

  server.on(
    "secureConnection",
    (
        (sock: TlsSocket) =>
          if waiters.nonEmpty then waiters.dequeue()(ZIO.succeed(NodeConn.tls(sock)))
          else pending.enqueue(sock)
    ): js.Function1[TlsSocket, Unit],
  )
  server.on(
    "close",
    (() =>
      closed = true
      while waiters.nonEmpty do waiters.dequeue()(ZIO.fail(java.io.IOException("listener closed")))
      closeWaiter.foreach(_())
      closeWaiter = None
    ): js.Function0[Unit],
  )

  def localPort: UIO[Int] =
    ZIO.succeed {
      val addr = server.address()
      if addr == null then 0
      else
        try addr.asInstanceOf[AddressInfo].port
        catch case _: Throwable => 0
    }

  def accept: Task[ByteConn] =
    ZIO.suspendSucceed {
      if pending.nonEmpty then ZIO.succeed(NodeConn.tls(pending.dequeue()))
      else if closed then ZIO.fail(java.io.IOException("listener closed"))
      else
        ZIO.async[Any, Throwable, ByteConn] { cb =>
          if pending.nonEmpty then cb(ZIO.succeed(NodeConn.tls(pending.dequeue())))
          else if closed then cb(ZIO.fail(java.io.IOException("listener closed")))
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
end TlsListener

object TlsListener:
  def bind(config: Server.Config, certPem: String, keyPem: String): IO[ServerError, Listener] =
    ZIO.async[Any, ServerError, Listener] { cb =>
      val alpn =
        if config.http2 then js.Array("h2", "http/1.1")
        else js.Array("http/1.1")
      val opts   = TlsOptions(key = keyPem, cert = certPem, ALPNProtocols = alpn)
      val server = NodeTls.createServer(opts, (_: TlsSocket) => ())
      server.once(
        "error",
        (
            (err: js.Error) =>
              cb(
                ZIO.fail(
                  ServerError.BindFailed(
                    config.host,
                    config.port,
                    java.io.IOException(Option(err.message).getOrElse("tls listen failed")),
                  )
                )
              )
        ): js.Function1[js.Error, Unit],
      )
      val _ = server.listen(
        config.port,
        config.host,
        () => cb(ZIO.succeed(TlsListener(server))),
      )
    }
end TlsListener
