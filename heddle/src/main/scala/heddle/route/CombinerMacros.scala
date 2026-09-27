package heddle.route

import scala.quoted.*

/** Builds the tuple-append `Combiner` for one concrete arity, so its values are real `TupleN`s and need no cast. */
private[route] object CombinerMacros:
  def append[T <: NonEmptyTuple: Type, B: Type](using Quotes): Expr[Combiner[T, B]] =
    import quotes.reflect.*
    val elems = elementsOf(TypeRepr.of[T])
    if elems.length >= 22 then report.errorAndAbort(s"an input of ${elems.length + 1} values is past the tuple limit")
    defn.TupleClass(elems.length + 1).typeRef.appliedTo(elems :+ TypeRepr.of[B]).asType match
      case '[o] =>
        def field(t: Term, i: Int): Term              = Select.unique(t, s"_${i + 1}")
        def combined(a: Expr[T], b: Expr[B]): Expr[o] =
          Expr.ofTupleFromSeq(elems.indices.map(i => field(a.asTerm, i).asExpr) :+ b).asExprOf[o]
        def separated(out: Expr[o]): Expr[(T, B)] =
          val init = Expr.ofTupleFromSeq(elems.indices.map(i => field(out.asTerm, i).asExpr)).asExprOf[T]
          val last = field(out.asTerm, elems.length).asExprOf[B]
          '{ ($init, $last) }
        '{
          new Combiner[T, B]:
            type Out = o
            def combine(a: T, b: B): o   = ${ combined('a, 'b) }
            def separate(out: o): (T, B) = ${ separated('out) }
        }
    end match
  end append

  private def elementsOf(using Quotes)(tuple: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tuple.dealias match
      case AppliedType(tc, args) if defn.isTupleClass(tc.typeSymbol)                                  => args
      case AppliedType(cons, List(head, tail)) if cons.typeSymbol == Symbol.requiredClass("scala.*:") =>
        head :: elementsOf(tail)
      case t if t =:= TypeRepr.of[EmptyTuple] => Nil
      case other => report.errorAndAbort(s"cannot add to ${other.show}: not a concrete tuple")
end CombinerMacros
