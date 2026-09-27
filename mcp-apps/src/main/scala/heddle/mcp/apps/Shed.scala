package heddle.mcp.apps

import heddle.endpoint.Endpoint
import scala.NamedTuple.NamedTuple

/** One tool a view may use: the shared `Endpoint` the server binds and the view calls, and who may call it. */
final case class Grant[In, Err, Out](endpoint: Endpoint[In, Err, Out], visibility: Visibility):
  def toolName: String = endpoint.doc.toolName

object Grant:
  /** The tool the model calls to open the view. The view may call it again to refresh. */
  def launch[In, Err, Out](endpoint: Endpoint[In, Err, Out]): Grant[In, Err, Out] =
    Grant(endpoint, Visibility.ModelAndApp)

  /** A tool only the view calls; the host keeps it from the model. */
  def app[In, Err, Out](endpoint: Endpoint[In, Err, Out]): Grant[In, Err, Out] =
    Grant(endpoint, Visibility.App)

/** Evidence that every field of a named tuple is a `Grant`. */
@scala.annotation.implicitNotFound("Every field of a shed's tools must be a Grant; ${V} has one that is not.")
sealed trait AllGrants[V <: Tuple]:
  def list(v: V): List[Grant[?, ?, ?]]

object AllGrants:
  given empty: AllGrants[EmptyTuple] with
    def list(v: EmptyTuple): List[Grant[?, ?, ?]] = Nil

  given cons[G <: Grant[?, ?, ?], T <: Tuple](using rest: AllGrants[T]): AllGrants[G *: T] with
    def list(v: G *: T): List[Grant[?, ?, ?]] = v.head :: rest.list(v.tail)

/** The set of grants one `ui://` view may use, and the policy it asks its host for. The view calls a tool by picking it
  * from `tools` (`bridge.call(_.inc)`), so a grant from another shed is not reachable.
  */
final case class Shed[N <: Tuple, V <: Tuple](
    uri: UiUri,
    title: String,
    launch: Grant[?, ?, ?],
    tools: NamedTuple[N, V],
    policy: UiPolicy,
)(using all: AllGrants[V]):
  type Tools = NamedTuple[N, V]

  /** The launch grant first, then the view's tools in declaration order. */
  def grants: List[Grant[?, ?, ?]] = launch :: all.list(tools.toTuple)

  def withPolicy(next: UiPolicy): Shed[N, V] = copy(policy = next)
end Shed

object Shed:
  /** Pins the uri, title, and launch tool; `apply` takes the view's tools as a named tuple. */
  def apply(uri: UiUri, title: String, launch: Grant[?, ?, ?]): Builder = Builder(uri, title, launch)

  final class Builder(uri: UiUri, title: String, launch: Grant[?, ?, ?]):
    def apply[N <: Tuple, V <: Tuple](tools: NamedTuple[N, V])(using AllGrants[V]): Shed[N, V] =
      Shed(uri, title, launch, tools, UiPolicy.closed)

  /** A view with no tools of its own: it renders the launch result and calls nothing. */
  def only(uri: UiUri, title: String, launch: Grant[?, ?, ?]): Shed[EmptyTuple, EmptyTuple] =
    Shed(uri, title, launch, NamedTuple.Empty, UiPolicy.closed)
end Shed
