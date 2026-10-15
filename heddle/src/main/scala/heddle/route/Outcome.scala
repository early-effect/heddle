package heddle.route

import heddle.http.{Response, Status}
import zio.{UIO, ZIO}

/** What a `Routes` value did with a request, before anyone turns a miss into an HTTP response.
  *
  * `Miss` and `NotAllowed` are routing. `Answered` is a response a matched route produced, whatever its status.
  * `decorate` runs only when this outcome is the one that gets rendered, so a later `++` can still discard a miss.
  */
private[heddle] enum Outcome:
  case Miss(decorate: Response => UIO[Response])
  case NotAllowed(allow: String, decorate: Response => UIO[Response])
  case Answered(response: Response)

  def status: Status =
    this match
      case Answered(response) => response.status
      case Miss(_)            => Status.NotFound
      case NotAllowed(_, _)   => Status.MethodNotAllowed
end Outcome

private[heddle] object Outcome:
  val miss: Outcome = Miss(ZIO.succeed(_))

  def notAllowed(allow: String): Outcome = NotAllowed(allow, ZIO.succeed(_))

  def render(outcome: Outcome): UIO[Response] =
    outcome match
      case Answered(response)          => ZIO.succeed(response)
      case Miss(decorate)              => decorate(Response.notFound())
      case NotAllowed(allow, decorate) => decorate(Response.methodNotAllowed(allow))

  /** `f` sees an answered response now. On a miss or a 405 it waits until render, so `++` can still discard it. */
  def map(outcome: Outcome)(f: Response => Response): Outcome =
    outcome match
      case Answered(response)          => Answered(f(response))
      case Miss(decorate)              => Miss(response => decorate(response).map(f))
      case NotAllowed(allow, decorate) => NotAllowed(allow, response => decorate(response).map(f))

  def pipe[R](outcome: Outcome)(f: Response => ZIO[R, Nothing, Response]): ZIO[R, Nothing, Outcome] =
    def later(decorate: Response => UIO[Response]): ZIO[R, Nothing, Response => UIO[Response]] =
      ZIO.environment[R].map { env => response =>
        decorate(response).flatMap(res => f(res).provideEnvironment(env))
      }
    outcome match
      case Answered(response)          => f(response).map(Answered(_))
      case Miss(decorate)              => later(decorate).map(Miss(_))
      case NotAllowed(allow, decorate) => later(decorate).map(NotAllowed(allow, _))
end Outcome
