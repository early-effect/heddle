package heddle.endpoint

import java.util.UUID
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

  given [A: Schema]: Schema[Option[A]] =
    of(SchemaDoc.Optional(summon[Schema[A]].doc))

  given [A: Schema]: Schema[List[A]] =
    of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: Schema]: Schema[Vector[A]] =
    of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: Schema]: Schema[Chunk[A]] =
    of(SchemaDoc.Array(summon[Schema[A]].doc))

  given [A: Schema, B: Schema]: Schema[Either[A, B]] =
    of(SchemaDoc.OneOf(None, List(summon[Schema[A]].doc, summon[Schema[B]].doc)))

  inline def derived[A](using m: Mirror.Of[A]): Schema[A] =
    inline m match
      case p: Mirror.ProductOf[A] => productSchema[A](p)
      case s: Mirror.SumOf[A]     => sumSchema[A](s)

  inline def productSchema[A](p: Mirror.ProductOf[A]): Schema[A] =
    val title   = constValue[p.MirroredLabel]
    val labels  = labelsOf[p.MirroredElemLabels]
    val schemas = schemasOf[p.MirroredElemTypes]
    val fields  = labels.zip(schemas).map { (name, schema) =>
      val (doc, optional) = schema.doc.unwrapOptional
      SchemaField(name, doc, optional)
    }
    val required = fields.filterNot(_.optional).map(_.name)
    Schema.of(SchemaDoc.Object(Some(title), fields, required), Some(title))
  end productSchema

  /** Matches zio-json's derived sum encoding: `{"Case": {...}}` per leaf case, or a bare `"Case"` string when every
    * leaf is a singleton. Nested sealed hierarchies flatten to their leaves, as zio-json does.
    */
  inline def sumSchema[A](s: Mirror.SumOf[A]): Schema[A] =
    val title = constValue[s.MirroredLabel]
    if allSingletons[s.MirroredElemTypes] then
      val labels = leafLabelsOf[s.MirroredElemTypes, s.MirroredElemLabels]
      sum(SchemaDoc.Enum(Some(title), labels.flatten), title, labels.map(SchemaDoc.Enum(None, _)))
    else
      val perCase = wrappedOf[s.MirroredElemTypes, s.MirroredElemLabels]
      sum(SchemaDoc.OneOf(Some(title), perCase), title, perCase)
  end sumSchema

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
      case _                  => List(constValue[L].asInstanceOf[String])
    }

  private inline def wrappedOf[T <: Tuple, L <: Tuple]: List[SchemaDoc] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => Nil
      case _: ((h *: t), (l *: ls))    => wrapped[h, l] :: wrappedOf[t, ls]

  private inline def wrapped[H, L]: SchemaDoc =
    summonFrom {
      case m: Mirror.SumOf[H] => SchemaDoc.OneOf(None, wrappedOf[m.MirroredElemTypes, m.MirroredElemLabels])
      case _                  =>
        val label = constValue[L].asInstanceOf[String]
        SchemaDoc.Object(None, List(SchemaField(label, schemaOf[H].doc, optional = false)), List(label))
    }

  private inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: labelsOf[t]

  private inline def schemasOf[T <: Tuple]: List[Schema[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => schemaOf[h] :: schemasOf[t]

  private inline def schemaOf[H]: Schema[H] =
    summonFrom {
      case s: Schema[H]           => s
      case m: Mirror.ProductOf[H] => productSchema[H](m)
      case m: Mirror.SumOf[H]     => sumSchema[H](m)
    }
end Schema
