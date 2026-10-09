package heddle.route

import heddle.http.{Method, Request, Response}
import zio.*

/** One method and path, and what answers it. `Params` is what the path captures; `run` takes exactly that. */
sealed trait Route[-R, +E]:
  type Params
  def method: Method
  def path: PathCodec[Params]
  def run(params: Params, request: Request): ZIO[R, E, Response]

  /** The captured params when the request's path matches, then the handler. */
  private[heddle] def dispatch(request: Request): Option[ZIO[R, E, Response]] =
    path.literalMatch.orElse(path.matches(request.path)).map(run(_, request))

  def catchAll[R1 <: R, E2](f: E => ZIO[R1, E2, Response]): Route[R1, E2] =
    Route.from(method, path, (params, request) => run(params, request).catchAll(f))
end Route

object Route:
  def from[A, R, E](method: Method, path: PathCodec[A], f: (A, Request) => ZIO[R, E, Response]): Route[R, E] =
    Of(method, path, f)

  private final case class Of[A, -R, +E](
      method: Method,
      path: PathCodec[A],
      f: (A, Request) => ZIO[R, E, Response],
  ) extends Route[R, E]:
    type Params = A
    def run(params: A, request: Request): ZIO[R, E, Response] = f(params, request)
end Route

/** `Captures` is the path's names, in order. It is not named `Names`: that is the codec's type member, and a refinement
  * `type Names = Names` would point at itself.
  */
final case class RoutePattern[Captures <: Tuple, A](
    method: Method,
    path: PathCodec[A] { type Names = Captures },
):
  inline def /(inline lit: String): RoutePattern[Captures, A] =
    RoutePattern(method, path.appendLit[Captures](lit))

  inline def /[M <: Tuple, B](codec: PathCodec[B] { type Names = M })(using
      c: Combiner[A, B]
  ): RoutePattern[Tuple.Concat[Captures, M], c.Out] =
    NamesCheck.distinct[Captures, M]
    RoutePattern(method, path.joined[Captures, M, B](codec))

  def ->[R, E](h: Handler[R, E])(using ev: A =:= Unit): Route[R, E] =
    Route.from(method, ev.substituteCo[PathCodec](path), (_, req) => h.run(req))

  /** The captures, in path order. The parameter names are the names in the path. */
  inline def ->[R, E](inline f: A => ZIO[R, E, Response]): Route[R, E] =
    ${ BindNames.arrow[Captures, A, R, E]('this, 'f) }

  /** The captures, in path order, then the request. The request's own name is not a capture. */
  inline def handle[O, R, E](using c: Combiner[A, Request] { type Out = O })(
      inline f: O => ZIO[R, E, Response]
  ): Route[R, E] =
    ${ BindNames.handle[Captures, A, O, R, E]('this, 'c, 'f) }

  def ->[R, E](response: Response)(using A =:= Unit): Route[R, E] =
    this -> Handler.succeed(response)
end RoutePattern
