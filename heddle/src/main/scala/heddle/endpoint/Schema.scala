package heddle.endpoint

import java.util.UUID
import scala.compiletime.{constValue, erasedValue, summonInline}
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
  case Optional(inner: SchemaDoc)
  case Ref(name: String)

  def unwrapOptional: (SchemaDoc, Boolean) =
    this match
      case Optional(inner) => (inner, true)
      case other           => (other, false)
end SchemaDoc

trait Schema[A]:
  def doc: SchemaDoc
  def named: Option[String] = None

object Schema:
  def of[A](d: SchemaDoc, name: Option[String] = None): Schema[A] =
    new Schema[A]:
      def doc: SchemaDoc                 = d
      override def named: Option[String] = name

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

  inline def sumSchema[A](s: Mirror.SumOf[A]): Schema[A] =
    val title    = constValue[s.MirroredLabel]
    val variants = schemasOf[s.MirroredElemTypes].map(_.doc)
    Schema.of(SchemaDoc.OneOf(Some(title), variants), Some(title))

  private inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: labelsOf[t]

  private inline def schemasOf[T <: Tuple]: List[Schema[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[Schema[h]] :: schemasOf[t]
end Schema
