package heddle.route

import heddle.http.{Method, Request, Response}
import zio.*

final case class Route[-R, +E](
    method: Method,
    path: PathCodec[?],
    run: (Any, Request) => ZIO[R, E, Response],
)

object Route:
  def from[A, R, E](
      method: Method,
      path: PathCodec[A],
      f: (A, Request) => ZIO[R, E, Response],
  ): Route[R, E] =
    Route(method, path, (a, req) => f(a.asInstanceOf[A], req))

final case class RoutePattern[A](method: Method, path: PathCodec[A]):
  def /(lit: String): RoutePattern[A] = copy(path = path / lit)

  def /[B](codec: PathCodec[B]): RoutePattern[Combine[A, B]] =
    RoutePattern(method, path / codec)

  def ->[R, E](h: Handler[R, E])(using A =:= Unit): Route[R, E] =
    Route.from(method, path.asInstanceOf[PathCodec[Unit]], (_, req) => h.run(req))

  def ->[R, E](f: A => ZIO[R, E, Response]): Route[R, E] =
    Route.from(method, path, (a, _) => f(a))

  def ->[R, E](f: (A, Request) => ZIO[R, E, Response]): Route[R, E] =
    Route.from(method, path, f)

  def ->[R, E](response: Response)(using A =:= Unit): Route[R, E] =
    this -> Handler.succeed(response)
end RoutePattern
