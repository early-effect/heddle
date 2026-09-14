package heddle.endpoint

import heddle.http.{Request, Response}
import heddle.route.Routes
import zio.{IO, ZIO}

final class BoundOp[-R, In, Err, Out](
    val endpoint: Endpoint[In, Err, Out],
    val run: In => ZIO[R, Err, Out],
    inner: Routes[R, Response],
) extends Routes[R, Response]:
  def toChunk                                   = inner.toChunk
  def apply(request: Request)                   = inner(request)
  def input(request: Request): IO[Response, In] =
    endpoint.path.matches(request.path) match
      case None    => ZIO.fail(Response.notFound())
      case Some(p) => endpoint.decodeIn(p, request)
end BoundOp
