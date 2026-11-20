package heddle.endpoint

import heddle.http.Response
import heddle.route.Routes
import zio.stream.ZStream
import zio.{Chunk, ZIO}

final class Api[-R](
    val title: String,
    val version: String,
    val ops: Chunk[BoundOp[R, ?, ?, ?]],
    val subscriptions: Chunk[BoundSubscription[R, ?, ?, ?]],
    val description: Option[String] = None,
):
  def bind[R1, In, Err, Out](ep: Endpoint[In, Err, Out])(f: In => ZIO[R1, Err, Out]): Api[R & R1] =
    val bound: BoundOp[R & R1, In, Err, Out] = ep.implement(f)
    new Api(title, version, ops :+ bound, subscriptions, description)

  /** Binds a subscription. `open` fails with the operation's error, or returns the stream one listener pulls. */
  def stream[R1, In, Err, A](sub: Subscription[In, Err, A])(
      open: In => ZIO[R1, Err, ZStream[R1, Err, Observed[A]]]
  ): Api[R & R1] =
    val bound: BoundSubscription[R & R1, In, Err, A] = sub.bind(open)
    new Api(title, version, ops, subscriptions :+ bound, description)

  def resource[R1, In, Err, Out](ep: Endpoint[In, Err, Out])(f: In => ZIO[R1, Err, Out]): Api[R & R1] =
    bind(ep.unpromote)(f)

  def job[R1, In, Err, Out](ep: Endpoint[In, Err, Out])(f: In => ZIO[R1, Err, Out]): Api[R & R1] =
    bind(if ep.doc.promoted then ep else ep.mcp)(f)

  def routes: Routes[R, Response] =
    val values = ops.foldLeft[Routes[R, Response]](Routes.empty)(_ ++ _)
    subscriptions.foldLeft[Routes[R, Response]](values)(_ ++ _)

  def openApi: OpenApi =
    OpenApi(title, version, documented.toList, description)

  private def documented: Chunk[EndpointDoc] =
    ops.map(_.endpoint.doc) ++ subscriptions.map(_.subscription.doc)
end Api

object Api:
  def apply(title: String, version: String, description: Option[String] = None): Api[Any] =
    new Api(title, version, Chunk.empty, Chunk.empty, description)

  def openApi(title: String, version: String, apis: Api[?]*): OpenApi =
    OpenApi(
      title,
      version,
      apis.toList.flatMap(api => api.ops.map(_.endpoint.doc) ++ api.subscriptions.map(_.subscription.doc)),
    )
end Api
