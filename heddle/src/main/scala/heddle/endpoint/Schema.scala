package heddle.endpoint

import java.util.UUID
import scala.collection.mutable
import scala.deriving.Mirror
import scala.quoted.*
import zio.Chunk

final case class SchemaField(name: String, doc: SchemaDoc, optional: Boolean)

enum SchemaDoc:
  case Null
  case Boolean
  case Integer(format: Option[String])
  case Number(format: Option[String])
  case Str(format: Option[String])
  case Array(items: SchemaDoc)
  case Object(title: Option[String], fields: List[SchemaField], required: List[String])
  case OneOf(title: Option[String], variants: List[SchemaDoc])
  case Enum(title: Option[String], values: List[String])
  case Optional(inner: SchemaDoc)
  case Ref(name: String)

  def unwrapOptional: (SchemaDoc, Boolean) =
    this match
      case Optional(inner) => (inner, true)
      case other           => (other, false)

  def jsonSchema: zio.json.ast.Json = SchemaJson.render(this)
end SchemaDoc

trait Schema[A]:
  def doc: SchemaDoc
  def named: Option[String] = None

  /** Wire schema of each case of a sum, indexed by `Mirror.SumOf[A].ordinal`. Empty for non-sums. */
  def cases: List[SchemaDoc] = Nil
end Schema

object Schema:
  def of[A](d: SchemaDoc, name: Option[String] = None): Schema[A] =
    new Schema[A]:
      def doc: SchemaDoc                 = d
      override def named: Option[String] = name

  private def sum[A](d: SchemaDoc, name: String, perCase: List[SchemaDoc]): Schema[A] =
    new Schema[A]:
      def doc: SchemaDoc                  = d
      override def named: Option[String]  = Some(name)
      override def cases: List[SchemaDoc] = perCase

  given Schema[String]  = of(SchemaDoc.Str(None))
  given Schema[Int]     = of(SchemaDoc.Integer(Some("int32")))
  given Schema[Long]    = of(SchemaDoc.Integer(Some("int64")))
  given Schema[Double]  = of(SchemaDoc.Number(Some("double")))
  given Schema[Float]   = of(SchemaDoc.Number(Some("float")))
  given Schema[Boolean] = of(SchemaDoc.Boolean)
  given Schema[UUID]    = of(SchemaDoc.Str(Some("uuid")))
  given Schema[Unit]    = of(SchemaDoc.Null)

  given [A](using schema: Schema[A]): Schema[Option[A]] =
    defer(schema)(SchemaDoc.Optional(_))

  given [A](using schema: Schema[A]): Schema[List[A]] =
    arrayOf(schema)

  given [A](using schema: Schema[A]): Schema[Vector[A]] =
    arrayOf(schema)

  given [A](using schema: Schema[A]): Schema[Chunk[A]] =
    arrayOf(schema)

  given [A, B](using left: Schema[A], right: Schema[B]): Schema[Either[A, B]] =
    new Deferred[Either[A, B]](None):
      def build(forcing: List[Schema[?]]): Slice =
        val (l, lh) = readSchema(left, forcing)
        val (r, rh) = readSchema(right, forcing)
        (SchemaDoc.OneOf(None, List(l, r)), lh ::: rh)

  inline def derived[A](using m: Mirror.Of[A]): Schema[A] =
    ${ deriveMacro[A]('m) }

  /** zio-json writes an all-singleton sum as a bare case name and every other sum as `{"Case": ...}`. Nested sums
    * flatten. The macro builds one plan, so a sum derived in place matches `Schema.derived` on that sum.
    */
  private enum Plan:
    case Use(schema: Schema[?])
    // Read a summoned schema when the document is forced. An eager read deadlocks two `derives` givens that name each other.
    case Delay(load: () => Schema[?])
    case Self(title: String)
    case Product(title: String, fields: List[FieldPlan])
    case EnumSum(title: String, groups: List[List[String]])
    case WrappedSum(title: String, cases: List[CasePlan])

  private final case class FieldPlan(name: String, plan: Plan)

  private enum CasePlan:
    case Leaf(label: String, inner: Plan)
    case Nested(cases: List[CasePlan])

  private def borrowed[A](schema: Schema[?]): Schema[A] =
    new Schema[A]:
      def doc: SchemaDoc                  = readSchema(schema, Nil)._1
      override def cases: List[SchemaDoc] = schema.cases

  private def interpret[A](plan: Plan): Schema[A] =
    plan match
      case Plan.EnumSum(title, groups) =>
        sum(SchemaDoc.Enum(Some(title), groups.flatten), title, groups.map(group => SchemaDoc.Enum(None, group)))
      case Plan.Product(title, fields) =>
        linked(
          title,
          isSum = false,
          forcing =>
            val (shaped, hits) = evalFields(fields, forcing)
            val required       = shaped.filterNot(_.optional).map(_.name)
            (SchemaDoc.Object(Some(title), shaped, required), hits),
        )
      case Plan.WrappedSum(title, cases) =>
        linked(
          title,
          isSum = true,
          forcing =>
            val (docs, hits) = evalCases(cases, forcing)
            (SchemaDoc.OneOf(Some(title), docs), hits),
        )
      case Plan.Use(schema) =>
        borrowed(schema)
      case Plan.Delay(load) =>
        new Schema[A]:
          private lazy val schema             = load()
          def doc: SchemaDoc                  = readSchema(schema, Nil)._1
          override def cases: List[SchemaDoc] = schema.cases
      case Plan.Self(title) =>
        of(SchemaDoc.Ref(title), Some(title))

  private def eval(plan: Plan, forcing: List[Schema[?]]): Slice =
    plan match
      case Plan.Use(schema)            => readSchema(schema, forcing)
      case Plan.Delay(load)            => readSchema(load(), forcing)
      case Plan.Self(title)            => (SchemaDoc.Ref(title), Nil)
      case Plan.EnumSum(title, groups) => (SchemaDoc.Enum(Some(title), groups.flatten), Nil)
      case Plan.Product(title, fields) =>
        val (shaped, hits) = evalFields(fields, forcing)
        val required       = shaped.filterNot(_.optional).map(_.name)
        (SchemaDoc.Object(Some(title), shaped, required), hits)
      case Plan.WrappedSum(title, cases) =>
        val (docs, hits) = evalCases(cases, forcing)
        (SchemaDoc.OneOf(Some(title), docs), hits)

  private def evalFields(
      fields: List[FieldPlan],
      forcing: List[Schema[?]],
  ): (List[SchemaField], List[Schema[?]]) =
    fields match
      case Nil                                => (Nil, Nil)
      case FieldPlan(name, plan) :: remaining =>
        val (doc, hits)       = eval(plan, forcing)
        val (inner, optional) = doc.unwrapOptional
        val (rest, restHits)  = evalFields(remaining, forcing)
        (SchemaField(name, inner, optional) :: rest, hits ::: restHits)

  private def evalCases(cases: List[CasePlan], forcing: List[Schema[?]]): (List[SchemaDoc], List[Schema[?]]) =
    cases match
      case Nil                                     => (Nil, Nil)
      case CasePlan.Leaf(label, plan) :: remaining =>
        val (doc, hits)      = eval(plan, forcing)
        val wrapped          = SchemaDoc.Object(None, List(SchemaField(label, doc, optional = false)), List(label))
        val (rest, restHits) = evalCases(remaining, forcing)
        (wrapped :: rest, hits ::: restHits)
      case CasePlan.Nested(nested) :: remaining =>
        val (docs, hits)     = evalCases(nested, forcing)
        val (rest, restHits) = evalCases(remaining, forcing)
        (SchemaDoc.OneOf(None, docs) :: rest, hits ::: restHits)

  private type Slice = (SchemaDoc, List[Schema[?]])

  private val componentBodies = mutable.Map.empty[String, SchemaDoc]

  private def publish(name: String, doc: SchemaDoc): Unit =
    componentBodies.synchronized {
      if !componentBodies.contains(name) then componentBodies.update(name, doc)
    }

  /** Body of a named schema whose use site is only a [[SchemaDoc.Ref]]. */
  private[heddle] def published(name: String): Option[SchemaDoc] =
    componentBodies.synchronized(componentBodies.get(name))

  private def arrayOf[C](schema: Schema[?]): Schema[C] =
    defer(schema)(SchemaDoc.Array(_))

  private def defer[C](schema: Schema[?])(wrap: SchemaDoc => SchemaDoc): Schema[C] =
    new Deferred[C](None):
      def build(forcing: List[Schema[?]]): Slice =
        val (inner, hits) = readSchema(schema, forcing)
        (wrap(inner), hits)

  private def linked[A](title: String, isSum: Boolean, describe: List[Schema[?]] => Slice): Schema[A] =
    new Deferred[A](Some(title)):
      def build(forcing: List[Schema[?]]): Slice = describe(forcing)
      override def cases: List[SchemaDoc]        =
        if isSum then
          doc match
            case SchemaDoc.OneOf(_, variants) => variants
            case _                            => Nil
        else Nil

  private def readSchema(schema: Schema[?], forcing: List[Schema[?]]): Slice =
    schema match
      case deferred: Deferred[?] =>
        val (doc, hits) = deferred.force(forcing)
        val reentered   = hits.exists(hit => forcing.exists(_ eq hit))
        deferred.componentName match
          case Some(name) if reentered && isComponent(doc) => (SchemaDoc.Ref(name), hits)
          case _                                           => (doc, hits)
      case other => (other.doc, Nil)

  private def isComponent(doc: SchemaDoc): Boolean =
    doc match
      case SchemaDoc.Object(Some(_), _, _) => true
      case SchemaDoc.OneOf(Some(_), _)     => true
      case SchemaDoc.Enum(Some(_), _)      => true
      case _                               => false

  /** `doc` is lazy so a mutual pair can finish initializing. Forcing a schema already on the stack yields `Ref`, not
    * another copy of its document.
    */
  private sealed abstract class Deferred[A](name: Option[String]) extends Schema[A]:
    lazy val doc: SchemaDoc            = force(Nil)._1
    override def named: Option[String] = name
    def componentName: Option[String]  = name

    def build(forcing: List[Schema[?]]): Slice

    private[Schema] def force(forcing: List[Schema[?]]): Slice =
      if forcing.exists(_ eq this) then
        name match
          case Some(value) => (SchemaDoc.Ref(value), this :: Nil)
          case None        => build(forcing)
      else
        val next          = if name.isDefined then this :: forcing else forcing
        val (built, hits) = build(next)
        name match
          case Some(value) if isComponent(built) => publish(value, built)
          case _                                 => ()
        (built, hits)
  end Deferred

  /** Picks a summoned schema over a mirror. Call-site givens, including the stubs around a recursive type, are visible
    * here. `summonFrom` has to live in an inline method: a quote typechecks too early to see those givens.
    */
  private inline def pickPlan[T](inline mirrored: Plan): Plan =
    scala.compiletime.summonFrom {
      case schema: Schema[T] => Plan.Use(schema)
      case _: Mirror.Of[T]   => mirrored
    }

  private inline def summonedPlan[T]: Plan =
    scala.compiletime.summonFrom { case schema: Schema[T] =>
      Plan.Use(schema)
    }

  private def deriveMacro[A: Type](mirror: Expr[Mirror.Of[A]])(using Quotes): Expr[Schema[A]] =
    import quotes.reflect.*

    final case class Frame(tpe: TypeRepr, title: String)
    final case class Info(
        label: String,
        elems: List[TypeRepr],
        labels: List[String],
        isSum: Boolean,
        singleton: Boolean,
    )

    def unwrap(tpe: TypeRepr): TypeRepr =
      tpe.dealias match
        case TypeBounds(_, hi) => unwrap(hi)
        case other             => other

    def member(tpe: TypeRepr, name: String): Option[TypeRepr] =
      def loop(current: TypeRepr, fuel: Int): Option[TypeRepr] =
        if fuel == 0 then None
        else
          current match
            case Refinement(_, `name`, info) => Some(info)
            case Refinement(parent, _, _)    => loop(parent, fuel - 1)
            case AndType(left, right)        => loop(left, fuel - 1).orElse(loop(right, fuel - 1))
            case AnnotatedType(inner, _)     => loop(inner, fuel - 1)
            case _                           => None
      loop(tpe.widen, 64)

    def tupleElems(tpe: TypeRepr): List[TypeRepr] =
      val current = unwrap(tpe)
      current match
        case AppliedType(cons, List(head, tail)) if cons.typeSymbol == Symbol.requiredClass("scala.*:") =>
          unwrap(head) :: tupleElems(tail)
        case AppliedType(fn, args) if defn.isTupleClass(fn.typeSymbol) =>
          args.map(unwrap)
        case empty if empty =:= TypeRepr.of[EmptyTuple] => Nil
        case other                                      =>
          report.errorAndAbort(s"expected a mirror tuple, found ${other.show}")

    def constString(tpe: TypeRepr): String =
      unwrap(tpe) match
        case ConstantType(StringConstant(value)) => value
        case other => report.errorAndAbort(s"expected a constant string, found ${other.show}")

    def bases(tpe: TypeRepr): List[String] =
      tpe.widen.baseClasses.map(_.fullName)

    def readMirror(mirrorTpe: TypeRepr): Info =
      val label =
        member(mirrorTpe, "MirroredLabel").map(constString).getOrElse {
          report.errorAndAbort(s"mirror ${mirrorTpe.widen.show} has no MirroredLabel")
        }
      val elems  = member(mirrorTpe, "MirroredElemTypes").map(tupleElems).getOrElse(Nil)
      val labels = member(mirrorTpe, "MirroredElemLabels").map(tpe => tupleElems(tpe).map(constString)).getOrElse(Nil)
      if elems.length != labels.length then
        report.errorAndAbort(
          s"mirror ${mirrorTpe.widen.show} has ${elems.length} element types and ${labels.length} labels"
        )
      val names = bases(mirrorTpe)
      Info(
        label,
        elems,
        labels,
        names.contains("scala.deriving.Mirror$.Sum"),
        names.contains("scala.deriving.Mirror$.Singleton"),
      )

    def mirrorInfo(tpe: TypeRepr): Option[Info] =
      tpe.dealias.asType match
        case '[t] => Expr.summon[Mirror.Of[t]].map(expr => readMirror(expr.asTerm.tpe))

    def leafSingleton(tpe: TypeRepr): Boolean =
      mirrorInfo(tpe) match
        case Some(info) if info.isSum     => info.elems.forall(leafSingleton)
        case Some(info) if info.singleton => true
        case Some(_)                      => false
        case None                         =>
          tpe.asType match
            case '[t] => Expr.summon[ValueOf[t]].isDefined

    def leafGroup(tpe: TypeRepr, label: String): List[String] =
      mirrorInfo(tpe) match
        case Some(info) if info.isSum =>
          info.elems.zip(info.labels).flatMap((child, childLabel) => leafGroup(child, childLabel))
        case _ => List(label)

    def frameOf(tpe: TypeRepr, stack: List[Frame]): Option[Frame] =
      val current = tpe.dealias
      stack.find(_.tpe =:= current)

    def mentions(tpe: TypeRepr, stack: List[Frame]): Boolean =
      val current = tpe.dealias
      stack.exists(_.tpe =:= current) || (current match
        case AppliedType(_, args)    => args.exists(mentions(_, stack))
        case AndType(left, right)    => mentions(left, stack) || mentions(right, stack)
        case OrType(left, right)     => mentions(left, stack) || mentions(right, stack)
        case AnnotatedType(inner, _) => mentions(inner, stack)
        case ByNameType(inner)       => mentions(inner, stack)
        case _                       => false)

    def summonSchema(tpe: TypeRepr): Option[Expr[Schema[?]]] =
      tpe.asType match
        case '[t] => Expr.summon[Schema[t]].map(_.asExprOf[Schema[?]])

    def withStubs(stack: List[Frame], body: Expr[Plan]): Expr[Plan] =
      stack.foldRight(body) { (frame, acc) =>
        frame.tpe.asType match
          case '[t] =>
            '{
              @scala.annotation.unused
              given Schema[t] =
                Schema.of[t](SchemaDoc.Ref(${ Expr(frame.title) }), Some(${ Expr(frame.title) }))
              $acc
            }
      }

    def choose[T: Type](stack: List[Frame], mirrored: Expr[Plan]): Expr[Plan] =
      withStubs(stack, '{ Schema.pickPlan[T]($mirrored) })

    def summonWrapped[T: Type](stack: List[Frame]): Expr[Plan] =
      withStubs(stack, '{ Schema.summonedPlan[T] })

    def missing(tpe: TypeRepr, stack: List[Frame], field: String): Nothing =
      val parent = stack.headOption.fold("derived type")(_.title)
      report.errorAndAbort(s"Cannot derive Schema for field $field of $parent: ${tpe.show}")

    def planType(tpe: TypeRepr, stack: List[Frame], field: String): Expr[Plan] =
      frameOf(tpe, stack) match
        case Some(frame) => '{ Schema.Plan.Self(${ Expr(frame.title) }) }
        case None        =>
          val current = tpe.dealias
          if mentions(current, stack) then
            current.asType match
              case '[t] =>
                mirrorInfo(current) match
                  case Some(info) => choose[t](stack, structure(info, stack, current))
                  case None       => summonWrapped[t](stack)
          else
            summonSchema(current) match
              case Some(schema) => '{ Schema.Plan.Delay(() => $schema) }
              case None         =>
                mirrorInfo(current) match
                  case Some(info) => structure(info, stack, current)
                  case None       => missing(current, stack, field)

    def casePlans(elems: List[TypeRepr], labels: List[String], stack: List[Frame]): List[Expr[CasePlan]] =
      elems.zip(labels).map { (tpe, label) =>
        (mirrorInfo(tpe), frameOf(tpe, stack)) match
          case (Some(info), None) if info.isSum =>
            '{ Schema.CasePlan.Nested(${ Expr.ofList(casePlans(info.elems, info.labels, stack)) }) }
          case (_, Some(frame)) =>
            '{ Schema.CasePlan.Leaf(${ Expr(label) }, Schema.Plan.Self(${ Expr(frame.title) })) }
          case _ =>
            '{ Schema.CasePlan.Leaf(${ Expr(label) }, ${ planType(tpe, stack, label) }) }
      }

    def fieldPlans(names: List[String], types: List[TypeRepr], stack: List[Frame]): List[Expr[FieldPlan]] =
      names.zip(types).map { (name, tpe) =>
        '{ Schema.FieldPlan(${ Expr(name) }, ${ planType(tpe, stack, name) }) }
      }

    def enumSum(title: String, groups: List[List[String]]): Expr[Plan] =
      val groupExprs = groups.map(group => Expr.ofList(group.map(Expr(_))))
      '{ Schema.Plan.EnumSum(${ Expr(title) }, ${ Expr.ofList(groupExprs) }) }

    def structure(info: Info, stack: List[Frame], self: TypeRepr): Expr[Plan] =
      val next = Frame(self.dealias, info.label) :: stack
      if info.isSum && info.elems.forall(leafSingleton) then
        enumSum(info.label, info.elems.zip(info.labels).map((tpe, label) => leafGroup(tpe, label)))
      else if info.isSum then
        '{ Schema.Plan.WrappedSum(${ Expr(info.label) }, ${ Expr.ofList(casePlans(info.elems, info.labels, next)) }) }
      else '{ Schema.Plan.Product(${ Expr(info.label) }, ${ Expr.ofList(fieldPlans(info.labels, info.elems, next)) }) }

    val self = TypeRepr.of[A].dealias
    val plan = structure(readMirror(mirror.asTerm.tpe), Nil, self)
    '{ Schema.interpret[A]($plan) }
  end deriveMacro
end Schema
