package heddle.route

import heddle.http.{Request, Response, Status}
import scala.collection.mutable
import zio.*

abstract class Routes[-R, +E]:
  def toChunk: Chunk[Route[R, E]]

  def apply(request: Request): ZIO[R, E, Response]

  def runZIO(request: Request): ZIO[R, E, Response] = apply(request)

  def ++[R1 <: R, E1 >: E](that: Routes[R1, E1]): Routes[R1, E1] =
    Routes.orElseNotFound(this, that)

  def @@[R1](middleware: Middleware[R1]): Routes[R & R1, E] =
    middleware(this)

  def handleError(f: E => Response): Routes[R, Nothing] =
    Routes.fromChunk(toChunk.map(_.catchAll(e => ZIO.succeed(f(e)))))

  def handleErrorZIO[R1 <: R](f: E => ZIO[R1, Nothing, Response]): Routes[R1, Nothing] =
    Routes.fromChunk(toChunk.map(_.catchAll(f)))
end Routes

object Routes:
  def empty: Routes[Any, Nothing] = fromChunk(Chunk.empty)

  def apply[R, E](routes: Route[R, E]*): Routes[R, E] =
    fromChunk(Chunk.fromIterable(routes))

  def fromChunk[R, E](routes: Chunk[Route[R, E]]): Routes[R, E] =
    Impl(routes, Dispatch(routes))

  def wrap[R, E](inner: Routes[R, E])(around: Handler[R, E] => Handler[R, E]): Routes[R, E] =
    Impl(inner.toChunk, around(Handler(inner.apply)).run)

  def fromHandler[R, E](handler: Handler[R, E]): Routes[R, E] =
    Impl(Chunk.empty, handler.run)

  private def orElseNotFound[R, E](left: Routes[R, E], right: Routes[R, E]): Routes[R, E] =
    Impl(
      left.toChunk ++ right.toChunk,
      req =>
        left(req).flatMap { res =>
          res.status match
            case Status.NotFound         => right(req)
            case Status.MethodNotAllowed =>
              right(req).map { r2 =>
                r2.status match
                  case Status.NotFound         => res
                  case Status.MethodNotAllowed =>
                    Response.methodNotAllowed(mergeAllow(res.header("Allow"), r2.header("Allow")))
                  case _ => r2
              }
            case _ => ZIO.succeed(res)
        },
    )

  private def mergeAllow(left: Option[String], right: Option[String]): String =
    (left.toList ++ right.toList)
      .flatMap(_.split(","))
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct
      .mkString(", ")

  private final class Impl[-R, +E](
      val toChunk: Chunk[Route[R, E]],
      dispatch: Request => ZIO[R, E, Response],
  ) extends Routes[R, E]:
    def apply(request: Request): ZIO[R, E, Response] = dispatch(request)

  extension [R, E](self: Routes[R, E])
    /** Runs `extract` first and hands its `P` to the routes, which then need only `R0`. A failed extract is the
      * response. `covers` is the compiler's proof that `P` and `R0` together are everything the routes need.
      */
    def provided[P: Tag, R0](extract: Request => ZIO[R0, Response, P])(using
        covers: (R0 & P) <:< R
    ): Routes[R0, E] =
      fromHandler(
        Handler { req =>
          extract(req).foldZIO(
            res => ZIO.succeed(res),
            p => self(req).provideSomeEnvironment[R0](env => covers.liftCo[ZEnvironment](env ++ ZEnvironment(p))),
          )
        }
      )
  end extension
end Routes

private[heddle] object Dispatch:
  def apply[R, E](routes: Chunk[Route[R, E]]): Request => ZIO[R, E, Response] =
    val tree = Node.build(routes)
    (req: Request) =>
      val found = tree.lookup(req.path.segments, 0)
      if found.isEmpty then ZIO.succeed(Response.notFound())
      else
        found.find(_.method == req.method) match
          case Some(route) => route.dispatch(req).getOrElse(ZIO.succeed(Response.notFound()))
          case None        =>
            val allow = found.map(_.method).distinct.map(_.render).mkString(", ")
            ZIO.succeed(Response.methodNotAllowed(allow))
      end if
  end apply

  private final class Node[-R, +E](
      val literals: Map[String, Node[R, E]],
      val variable: Option[Node[R, E]],
      val terminals: Chunk[Route[R, E]],
      val rest: Chunk[Route[R, E]],
  ):
    def lookup(parts: Chunk[String], from: Int): Chunk[Route[R, E]] =
      val here =
        if from >= parts.length then terminals
        else
          (literals.get(parts(from)), variable) match
            case (Some(lit), Some(vari)) => lit.lookup(parts, from + 1) ++ vari.lookup(parts, from + 1)
            case (Some(lit), None)       => lit.lookup(parts, from + 1)
            case (None, Some(vari))      => vari.lookup(parts, from + 1)
            case (None, None)            => Chunk.empty
      here ++ rest
    end lookup
  end Node

  private object Node:
    def build[R, E](routes: Chunk[Route[R, E]]): Node[R, E] =
      val root = MNode[R, E]()
      routes.foreach { route =>
        val (at, trailing) = route.path.segments.foldLeft((root, false)) { case ((cur, trailing), seg) =>
          seg match
            case Seg.Lit(value) => (cur.literals.getOrElseUpdate(value, MNode()), trailing)
            case Seg.Var(_, _)  => (cur.child, trailing)
            case Seg.Rest       =>
              cur.rest += route
              (cur, true)
        }
        if !trailing then at.terminals += route
      }
      root.freeze
    end build

    private final class MNode[R, E]:
      val literals: mutable.Map[String, MNode[R, E]]  = mutable.Map.empty
      val terminals: mutable.ArrayBuffer[Route[R, E]] = mutable.ArrayBuffer.empty
      val rest: mutable.ArrayBuffer[Route[R, E]]      = mutable.ArrayBuffer.empty
      private var variable: Option[MNode[R, E]]       = None

      def child: MNode[R, E] =
        variable.getOrElse {
          val made = MNode[R, E]()
          variable = Some(made)
          made
        }

      def freeze: Node[R, E] =
        Node(
          literals = literals.iterator.map((k, v) => k -> v.freeze).toMap,
          variable = variable.map(_.freeze),
          terminals = Chunk.fromIterable(terminals),
          rest = Chunk.fromIterable(rest),
        )
    end MNode
  end Node
end Dispatch
