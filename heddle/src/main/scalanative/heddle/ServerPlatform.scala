package heddle

import heddle.error.ServerError
import heddle.http.Response
import heddle.route.Routes
import zio.*

private[heddle] object ServerPlatform:
  def sbtInterruptExit: UIO[Unit] = ZIO.unit

  def install[R](routes: Routes[R, Response], config: Server.Config): ZIO[R & Scope, ServerError, Server] =
    install(routes, config, JvmScheduler.Loom)

  def install[R](
      routes: Routes[R, Response],
      config: Server.Config,
      scheduler: JvmScheduler,
  ): ZIO[R & Scope, ServerError, Server] =
    val _ = scheduler
    ZIO.fail(
      ServerError.BindFailed(
        config.host,
        config.port,
        RuntimeException(s"Server.install is not implemented on this platform yet (${routes.toChunk.length})"),
      )
    )
  end install
end ServerPlatform
