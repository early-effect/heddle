package heddle.endpoint

import scala.deriving.Mirror
import scala.quoted.*
import zio.json.JsonCodec

/** Checks `outErrors` cases against `Mirror.SumOf[E]` and lays their statuses out by ordinal. */
private[heddle] object ErrorCaseMacros:
  inline def codec[E](inline cases: ErrorCase[? <: E]*)(using s: Schema[E], j: JsonCodec[E]): ErrorCodec[E] =
    ${ codecImpl[E]('cases, 's, 'j) }

  def codecImpl[E: Type](
      cases: Expr[Seq[ErrorCase[? <: E]]],
      schema: Expr[Schema[E]],
      codec: Expr[JsonCodec[E]],
  )(using q: Quotes): Expr[ErrorCodec[E]] =
    import q.reflect.*

    val listed = cases match
      case Varargs(es) => es.toList
      case _ => report.errorAndAbort("outErrors takes ErrorCase values listed inline, not a splatted Seq", cases)

    val errorName = Type.show[E]
    val mirror    = Expr
      .summon[Mirror.SumOf[E]]
      .getOrElse(report.errorAndAbort(s"outErrors needs a sealed trait or enum; $errorName is neither"))

    val (children, labels) = mirror.asTerm.tpe.widen.asType match
      case '[Mirror.SumOf[E] { type MirroredElemTypes = elems; type MirroredElemLabels = ls }] =>
        (typesOf[elems], labelsOf[ls])
      case _ => report.errorAndAbort(s"cannot read the cases of $errorName")

    val errorCaseSym = TypeRepr.of[ErrorCase[?]].typeSymbol

    val placed = listed.map { e =>
      val caseType = e.asTerm.tpe.widen.baseType(errorCaseSym) match
        case AppliedType(_, List(_: TypeBounds)) =>
          report.errorAndAbort(s"ErrorCase needs a concrete case of $errorName, got ${e.asTerm.tpe.widen.show}", e)
        case AppliedType(_, List(a)) => a
        case other => report.errorAndAbort(s"ErrorCase needs a concrete case of $errorName, got ${other.show}", e)
      val ordinal = children.indexWhere(c => caseType =:= c)
      if ordinal < 0 then
        report.errorAndAbort(s"${caseType.show} is not a case of $errorName. Cases: ${labels.mkString(", ")}", e)
      ordinal -> e
    }

    placed.groupBy(_._1).toList.sortBy(_._1).foreach { (ordinal, uses) =>
      if uses.length > 1 then report.errorAndAbort(s"${labels(ordinal)} is listed twice in outErrors", uses(1)._2)
    }

    val byOrdinal = placed.toMap
    val missing   = labels.indices.filterNot(byOrdinal.contains).map(labels)
    if missing.nonEmpty then
      report.errorAndAbort(s"outErrors[$errorName] is missing ErrorCase for: ${missing.mkString(", ")}", cases)

    val statuses = Expr.ofList(labels.indices.toList.map(i => '{ ${ byOrdinal(i) }.status }))
    '{ ErrorCodec.cases[E]($statuses, (e: E) => $mirror.ordinal(e))(using $schema, $codec) }
  end codecImpl

  private def typesOf[T: Type](using q: Quotes): List[q.reflect.TypeRepr] =
    import q.reflect.*
    Type.of[T] match
      case '[EmptyTuple] => Nil
      case '[h *: t]     => TypeRepr.of[h] :: typesOf[t]
      case _             => report.errorAndAbort(s"unexpected mirror element tuple ${Type.show[T]}")

  private def labelsOf[T: Type](using q: Quotes): List[String] =
    import q.reflect.*
    Type.of[T] match
      case '[EmptyTuple] => Nil
      case '[h *: t]     =>
        TypeRepr.of[h] match
          case ConstantType(StringConstant(label)) => label :: labelsOf[t]
          case other                               => report.errorAndAbort(s"unexpected mirror label ${other.show}")
      case _ => report.errorAndAbort(s"unexpected mirror label tuple ${Type.show[T]}")
end ErrorCaseMacros
