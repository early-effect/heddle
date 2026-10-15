package heddle.route

import heddle.http.{Request, Response}
import scala.collection.mutable
import zio.*

abstract class Routes[-R, +E]:
  def toChunk: Chunk[Route[R, E]]

  /** Routing outcome. A handler response, including 404 and 405, is `Answered`. Only a path or method miss is not. */
  private[heddle] def decide(request: Request): ZIO[R, E, Outcome]

  def apply(request: Request): ZIO[R, E, Response] =
    decide(request).flatMap(Outcome.render)

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
    decided(routes, Dispatch(routes))

  /** Seals `inner` behind `around`. The function sees rendered responses, so a 404 it returns does not fall through. */
  def wrap[R, E](inner: Routes[R, E])(around: Handler[R, E] => Handler[R, E]): Routes[R, E] =
    decided(inner.toChunk, req => around(Handler(inner.apply)).run(req).map(Outcome.Answered(_)))

  def fromHandler[R, E](handler: Handler[R, E]): Routes[R, E] =
    decided(Chunk.empty, req => handler(req).map(Outcome.Answered(_)))

  private[heddle] def decided[R, E](
      routes: Chunk[Route[R, E]],
      run: Request => ZIO[R, E, Outcome],
  ): Routes[R, E] =
    Impl(routes, run)

  private def orElseNotFound[R, E](left: Routes[R, E], right: Routes[R, E]): Routes[R, E] =
    decided(
      left.toChunk ++ right.toChunk,
      req =>
        left.decide(req).flatMap {
          case Outcome.Miss(_)              => right.decide(req)
          case Outcome.Answered(response)   => ZIO.succeed(Outcome.Answered(response))
          case Outcome.NotAllowed(allow, _) =>
            right.decide(req).map {
              case Outcome.Miss(_)              => Outcome.notAllowed(allow)
              case Outcome.NotAllowed(other, _) => Outcome.notAllowed(mergeAllow(allow, other))
              case answered                     => answered
            }
        },
    )

  private def mergeAllow(left: String, right: String): String =
    (left.split(',') ++ right.split(','))
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct
      .mkString(", ")

  private final class Impl[-R, +E](
      val toChunk: Chunk[Route[R, E]],
      run: Request => ZIO[R, E, Outcome],
  ) extends Routes[R, E]:
    private[heddle] def decide(request: Request): ZIO[R, E, Outcome] = run(request)

  extension [R, E](self: Routes[R, E])
    /** Runs `extract` first and hands its `P` to the routes, which then need only `R0`. A failed extract is the
      * response. `covers` is the compiler's proof that `P` and `R0` together are everything the routes need.
      */
    def provided[P: Tag, R0](extract: Request => ZIO[R0, Response, P])(using
        covers: (R0 & P) <:< R
    ): Routes[R0, E] =
      decided(
        Chunk.empty,
        req =>
          extract(req).foldZIO(
            res => ZIO.succeed(Outcome.Answered(res)),
            p =>
              self
                .decide(req)
                .provideSomeEnvironment[R0](env => covers.liftCo[ZEnvironment](env ++ ZEnvironment(p))),
          ),
      )
  end extension
end Routes

private[heddle] object Dispatch:
  def apply[R, E](routes: Chunk[Route[R, E]]): Request => ZIO[R, E, Outcome] =
    val tree = Node.build(routes)
    (req: Request) =>
      val found = tree.lookup(req.path.segments, 0)
      if found.isEmpty then ZIO.succeed(Outcome.miss)
      else
        val same = found.filter(_.method == req.method)
        firstDecoded(same, req) match
          case Some(effect) => effect.map(Outcome.Answered(_))
          case None         =>
            if same.nonEmpty then ZIO.succeed(Outcome.miss)
            else ZIO.succeed(Outcome.notAllowed(allowList(found)))
  end apply

  /** The first same-method route whose path codec matches. A codec miss is not a response, so the next candidate runs.
    */
  private def firstDecoded[R, E](routes: Chunk[Route[R, E]], req: Request): Option[ZIO[R, E, Response]] =
    def go(rest: Chunk[Route[R, E]]): Option[ZIO[R, E, Response]] =
      rest.headOption match
        case None        => None
        case Some(route) =>
          route.dispatch(req) match
            case Some(effect) => Some(effect)
            case None         => go(rest.drop(1))
    go(routes)

  private def allowList[R, E](routes: Chunk[Route[R, E]]): String =
    routes.map(_.method).distinct.map(_.render).mkString(", ")

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
