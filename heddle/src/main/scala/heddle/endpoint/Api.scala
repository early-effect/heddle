package heddle.endpoint

import heddle.http.Response
import heddle.route.Routes
import zio.{Chunk, ZIO}

final class Api[-R](
    val title: String,
    val version: String,
    val ops: Chunk[BoundOp[R, ?, ?, ?]],
    val description: Option[String] = None,
):
  def bind[R1, In, Err, Out](ep: Endpoint[In, Err, Out])(f: In => ZIO[R1, Err, Out]): Api[R & R1] =
    val bound: BoundOp[R & R1, In, Err, Out] = ep.implement(f)
    new Api(title, version, ops :+ bound, description)

  def resource[R1, In, Err, Out](ep: Endpoint[In, Err, Out])(f: In => ZIO[R1, Err, Out]): Api[R & R1] =
    bind(ep.unpromote)(f)

  def job[R1, In, Err, Out](ep: Endpoint[In, Err, Out])(f: In => ZIO[R1, Err, Out]): Api[R & R1] =
    bind(if ep.doc.promoted then ep else ep.mcp)(f)

  def routes: Routes[R, Response] =
    ops.foldLeft[Routes[R, Response]](Routes.empty)(_ ++ _)

  def openApi: OpenApi =
    OpenApi(title, version, ops.map(_.endpoint.doc).toList, description)
end Api

object Api:
  def apply(title: String, version: String, description: Option[String] = None): Api[Any] =
    new Api(title, version, Chunk.empty, description)

  def openApi(title: String, version: String, apis: Api[?]*): OpenApi =
    OpenApi(title, version, apis.toList.flatMap(_.ops.map(_.endpoint.doc).toList))
end Api
