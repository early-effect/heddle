package heddle

import zio.*

object LiveServer:
  def apply[E, A](routes: Routes[Any, Response], config: Server.Config = local)(
      f: String => IO[E, A]
  ): ZIO[Any, E | ServerError, A] =
    ZIO.scoped {
      Server.install(routes, config).flatMap { server =>
        server.port.flatMap(port => f(s"http://127.0.0.1:$port"))
      }
    }

  def https[E, A](
      routes: Routes[Any, Response],
      tls: ZLayer[Any, HttpError, Tls],
      config: Server.Config = local,
  )(f: String => IO[E, A]): ZIO[Any, E | HeddleError, A] =
    ZIO.scoped {
      Server.install(routes, config).provideSomeLayer[Scope](tls).flatMap { server =>
        server.port.flatMap(port => f(s"https://127.0.0.1:$port"))
      }
    }

  val local: Server.Config = Server.Config.default.copy(host = "127.0.0.1", port = 0)
end LiveServer
