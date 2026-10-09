package heddle.route

import scala.quoted.*

/** Reads the names of a lambda the compiler has already typed, and checks them against the path's capture names.
  *
  * A tuple lambda is untupled by the compiler into `val left = x$1._1`, in path order. Those val names are the binding.
  * A missing position is `_`. A `match` is rejected: a case lambda is one parameter, and its pattern is not the capture
  * list.
  */
private[route] object BindNames:
  enum Slot:
    case Named(name: String)
    case Skip

  def arrow[Captures <: Tuple: Type, A: Type, R: Type, E: Type](
      pattern: Expr[RoutePattern[Captures, A]],
      f: Expr[A => zio.ZIO[R, E, heddle.http.Response]],
  )(using Quotes): Expr[Route[R, E]] =
    check[Captures](f, request = false)
    '{ Route.from($pattern.method, $pattern.path, (a, _) => $f(a)) }

  def handle[Captures <: Tuple: Type, A: Type, O: Type, R: Type, E: Type](
      pattern: Expr[RoutePattern[Captures, A]],
      c: Expr[Combiner[A, heddle.http.Request] { type Out = O }],
      f: Expr[O => zio.ZIO[R, E, heddle.http.Response]],
  )(using Quotes): Expr[Route[R, E]] =
    check[Captures](f, request = true)
    '{ Route.from($pattern.method, $pattern.path, (a, req) => $f($c.combine(a, req))) }

  private def check[Captures <: Tuple: Type](f: Expr[Any], request: Boolean)(using Quotes): Unit =
    import quotes.reflect.*
    val names = readNames(TypeRepr.of[Captures])
    val named = names.exists { case Slot.Named(_) => true; case Slot.Skip => false }
    if named then
      val (arity, slots, matched) = readLambda(f.asTerm)
      val captureArity            = if request then arity - 1 else arity
      if matched || captureArity != names.length then mismatch(names, request)
      else
        val compared = slots.take(names.length)
        names.zip(compared).foreach {
          case (Slot.Skip, _)                                             => ()
          case (Slot.Named(_), None)                                      => ()
          case (Slot.Named(expected), Some(actual)) if actual == expected => ()
          case (Slot.Named(expected), Some(actual))                       =>
            report.errorAndAbort(s"capture $expected was bound as $actual. ${spell(names, request)}")
        }
      end if
    end if
  end check

  private def mismatch(names: List[Slot], request: Boolean)(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(spell(names, request))

  private def spell(names: List[Slot], request: Boolean): String =
    val captures = names.map { case Slot.Named(n) => n; case Slot.Skip => "_" }
    val params   = if request then captures :+ "req" else captures
    val form     =
      if params.length == 1 then s"${params.head} =>"
      else params.mkString("(", ", ", ") =>")
    s"Name each capture, in path order: $form"

  private def readNames(using Quotes)(tpe: quotes.reflect.TypeRepr): List[Slot] =
    import quotes.reflect.*
    def loop(t: TypeRepr): List[Slot] =
      t.dealias.simplified match
        case AppliedType(cons, List(head, tail)) if cons.typeSymbol.name == "*:" =>
          val slot = head.dealias.simplified match
            case ConstantType(StringConstant(s)) => Slot.Named(s)
            case h if h =:= TypeRepr.of[Unnamed] => Slot.Skip
            case other                           =>
              report.errorAndAbort(s"capture names are not in the type of this path (${other.show})")
          slot :: loop(tail)
        case done if done =:= TypeRepr.of[EmptyTuple] => Nil
        case other                                    =>
          report.errorAndAbort(s"capture names are not in the type of this path (${other.show})")
    loop(tpe)
  end readNames

  /** `(arity, slots by position, body is a match)`. A synthetic tuple parameter with no `_n` binding is all `_`. */
  private def readLambda(using Quotes)(term: quotes.reflect.Term): (Int, Vector[Option[String]], Boolean) =
    import quotes.reflect.*

    def unwrap(t: Term): Term = t match
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _)      => unwrap(inner)
      case Block(Nil, inner)    => unwrap(inner)
      case _                    => t

    def paramsOf(d: DefDef): List[ValDef] =
      d.paramss.flatMap {
        case TermParamClause(ps) => ps
        case _                   => Nil
      }

    val (params, rhs) = unwrap(term) match
      case Block(stats, Closure(_, _)) =>
        stats.collectFirst { case d: DefDef => (paramsOf(d), d.rhs) } match
          case Some(found) => found
          case None        => report.errorAndAbort("name each capture with a function literal")
      case Lambda(ps, body) => (ps, Some(body))
      case _                => report.errorAndAbort("name each capture with a function literal")

    val body = rhs match
      case Some(t) => unwrap(t)
      case None    => report.errorAndAbort("name each capture with a function literal")

    if params.length != 1 then
      val arity = params.length
      val slots = params.map(p => slotName(p.name)).toVector
      (arity, slots, false)
    else
      val param     = params.head
      val paramType = param.tpt.tpe.dealias.simplified
      val arity     =
        paramType match
          case AppliedType(tc, args) if defn.isTupleClass(tc.typeSymbol) => args.length
          case t if t =:= TypeRepr.of[Unit]                              => 0
          case _                                                         => 1
      body match
        case Match(_, _) => (arity, Vector.fill(arity)(None), true)
        case _           =>
          val bound = bindings(body, param.name)
          if arity > 1 && !synthetic(param.name) && bound.isEmpty then (arity, Vector.empty, true)
          else if bound.nonEmpty || synthetic(param.name) then
            (arity, Vector.tabulate(arity)(i => bound.get(i + 1).flatten), false)
          else (arity, Vector(slotName(param.name)), false)
    end if
  end readLambda

  private def slotName(name: String): Option[String] =
    if name == "_" || name.startsWith("_$") then None else Some(name)

  private def synthetic(name: String): Boolean =
    name.startsWith("x$") || name.startsWith("_$") || name == "_"

  /** Position (1-based) to the name bound from `param._n`. `None` is `_`. */
  private def bindings(using Quotes)(body: quotes.reflect.Term, param: String): Map[Int, Option[String]] =
    import quotes.reflect.*
    def index(name: String): Option[Int] =
      if name.startsWith("_") && name.drop(1).forall(_.isDigit) && name.length > 1 then Some(name.drop(1).toInt)
      else None
    def from(s: Statement): List[(Int, Option[String])] = s match
      case ValDef(name, _, Some(Select(Ident(qual), field))) if qual == param =>
        index(field).toList.map(i => i -> slotName(name))
      case Block(stats, expr) => stats.flatMap(from) ++ from(expr)
      case _                  => Nil
    from(body).toMap
  end bindings
end BindNames
