package heddle.endpoint

import java.util.UUID
import scala.annotation.unused
import scala.collection.mutable
import scala.compiletime.{constValue, erasedValue, summonFrom}
import scala.deriving.Mirror
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
    inline m match
      case p: Mirror.ProductOf[A] => productSchema[A](p)
      case s: Mirror.SumOf[A]     => sumSchema[A](s)

  inline def productSchema[A](p: Mirror.ProductOf[A]): Schema[A] =
    val title = constValue[p.MirroredLabel]
    // Visible only while this type is being described, so a recursive field is a Ref instead of another copy.
    // summonFrom inlines the given, which is why -Wunused cannot see the use.
    @unused given Schema[A] = of(SchemaDoc.Ref(title), Some(title))
    linked[A](title, isSum = false, forcing => productBody[A](p, forcing))

  /** Matches zio-json's derived sum encoding: `{"Case": {...}}` per leaf case, or a bare `"Case"` string when every
    * leaf is a singleton. Nested sealed hierarchies flatten to their leaves, as zio-json does.
    */
  inline def sumSchema[A](s: Mirror.SumOf[A]): Schema[A] =
    val title = constValue[s.MirroredLabel]
    if allSingletons[s.MirroredElemTypes] then
      val labels = leafLabelsOf[s.MirroredElemTypes, s.MirroredElemLabels]
      sum(SchemaDoc.Enum(Some(title), labels.flatten), title, labels.map(SchemaDoc.Enum(None, _)))
    else
      @unused given Schema[A] = of(SchemaDoc.Ref(title), Some(title))
      linked[A](
        title,
        isSum = true,
        forcing =>
          val (cases, hits) = wrappedOf[s.MirroredElemTypes, s.MirroredElemLabels](forcing)
          (SchemaDoc.OneOf(Some(title), cases), hits),
      )

  private inline def allSingletons[T <: Tuple]: Boolean =
    inline erasedValue[T] match
      case _: EmptyTuple => true
      case _: (h *: t)   => leafSingletons[h] && allSingletons[t]

  private inline def leafSingletons[H]: Boolean =
    summonFrom {
      case m: Mirror.SumOf[H] => allSingletons[m.MirroredElemTypes]
      case _: ValueOf[H]      => true
      case _                  => false
    }

  private inline def leafLabelsOf[T <: Tuple, L <: Tuple]: List[List[String]] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: ((h *: t), (l *: ls))    => leafLabels[h, l] :: leafLabelsOf[t, ls]

  private inline def leafLabels[H, L]: List[String] =
    summonFrom {
      case m: Mirror.SumOf[H] => leafLabelsOf[m.MirroredElemTypes, m.MirroredElemLabels].flatten
      case _                  => List(constValue[L].toString)
    }

  private inline def wrappedOf[T <: Tuple, L <: Tuple](
      forcing: List[Schema[?]]
  ): (List[SchemaDoc], List[Schema[?]]) =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => (Nil, Nil)
      case _: ((h *: t), (l *: ls))    =>
        val (doc, hits)      = wrapped[h, l](forcing)
        val (rest, restHits) = wrappedOf[t, ls](forcing)
        (doc :: rest, hits ::: restHits)

  private inline def wrapped[H, L](forcing: List[Schema[?]]): (SchemaDoc, List[Schema[?]]) =
    summonFrom {
      case m: Mirror.SumOf[H] =>
        val (cases, hits) = wrappedOf[m.MirroredElemTypes, m.MirroredElemLabels](forcing)
        (SchemaDoc.OneOf(None, cases), hits)
      case _ =>
        val label         = constValue[L].toString
        val (inner, hits) = read[H](forcing)
        (SchemaDoc.Object(None, List(SchemaField(label, inner, optional = false)), List(label)), hits)
    }

  private inline def productBody[A](p: Mirror.ProductOf[A], forcing: List[Schema[?]]): Slice =
    val title          = constValue[p.MirroredLabel]
    val (fields, hits) = fieldsOf[p.MirroredElemLabels, p.MirroredElemTypes](forcing)
    val required       = fields.filterNot(_.optional).map(_.name)
    (SchemaDoc.Object(Some(title), fields, required), hits)

  private inline def fieldsOf[L <: Tuple, T <: Tuple](forcing: List[Schema[?]]): (List[SchemaField], List[Schema[?]]) =
    inline erasedValue[(L, T)] match
      case _: (EmptyTuple, EmptyTuple) => (Nil, Nil)
      case _: ((l *: ls), (h *: hs))   =>
        val name              = constValue[l].toString
        val (doc, hits)       = read[h](forcing)
        val (inner, optional) = doc.unwrapOptional
        val (rest, restHits)  = fieldsOf[ls, hs](forcing)
        (SchemaField(name, inner, optional) :: rest, hits ::: restHits)

  /** Summon a finished schema, or describe a product or sum in place. A schema already on `forcing` is a `Ref`. */
  private inline def read[H](forcing: List[Schema[?]]): Slice =
    summonFrom {
      case s: Schema[H]           => readSchema(s, forcing)
      case m: Mirror.ProductOf[H] => productBody[H](m, forcing)
      case m: Mirror.SumOf[H]     =>
        val (cases, hits) = wrappedOf[m.MirroredElemTypes, m.MirroredElemLabels](forcing)
        (SchemaDoc.OneOf(None, cases), hits)
    }

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
end Schema
