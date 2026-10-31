package heddle

import zio.*
import zio.test.*

final case class Person(name: String, age: Option[Int]) derives Schema
final case class Team(people: List[Person]) derives Schema

object SchemaSpec extends ZIOSpecDefault:
  def spec =
    suite("Schema")(
      test("derives an object with optional fields omitted from required"):
        summon[Schema[Person]].doc match
          case SchemaDoc.Object(Some("Person"), fields, required) =>
            assertTrue(
              fields.map(_.name) == List("name", "age"),
              fields.find(_.name == "age").exists(_.optional),
              required == List("name"),
            )
          case _ => assertTrue(false)
      ,
      test("lists wrap the element schema"):
        summon[Schema[List[Int]]].doc match
          case SchemaDoc.Array(SchemaDoc.Integer(Some("int32"))) => assertTrue(true)
          case _                                                 => assertTrue(false)
      ,
      test("nested products keep their titles"):
        summon[Schema[Team]].doc match
          case SchemaDoc.Object(Some("Team"), fields, _) =>
            assertTrue(fields.headOption.exists(_.doc match
              case SchemaDoc.Array(SchemaDoc.Object(Some("Person"), _, _)) => true
              case _                                                       => false))
          case _ => assertTrue(false)
      ,
      test("payload enum derives a OneOf of label-wrapped cases"):
        enum Boom derives Schema:
          case Out(id: Int)
          case Gone
        val schema = summon[Schema[Boom]]
        schema.doc match
          case SchemaDoc.OneOf(Some("Boom"), variants) =>
            assertTrue(
              variants.map(wrapperLabel) == List(Some("Out"), Some("Gone")),
              schema.cases == variants,
            )
          case _ => assertTrue(false)
      ,
      test("all-singleton enum derives a string enum"):
        enum Flat derives Schema:
          case Up
          case Down
        val schema = summon[Schema[Flat]]
        assertTrue(
          schema.doc == SchemaDoc.Enum(Some("Flat"), List("Up", "Down")),
          schema.cases == List(SchemaDoc.Enum(None, List("Up")), SchemaDoc.Enum(None, List("Down"))),
        )
      ,
      test("nested sealed hierarchies flatten to their leaf labels"):
        val schema = summon[Schema[Outer]]
        schema.doc match
          case SchemaDoc.OneOf(Some("Outer"), List(inner, leaf)) =>
            assertTrue(
              wrapperLabel(leaf).contains("Leaf"),
              inner match
                case SchemaDoc.OneOf(None, vs) => vs.map(wrapperLabel) == List(Some("A"), Some("B"))
                case _                         => false,
            )
          case _ => assertTrue(false)
      ,
      test("case objects nested under a trait still count as singletons"):
        assertTrue(summon[Schema[Dir]].doc == SchemaDoc.Enum(Some("Dir"), List("North", "South", "East")))
      ,
      test("a recursive sum uses a ref at the back edge"):
        val child = field(summon[Schema[RecNode]].doc, "child")
        assertTrue(child.exists(f => !f.optional && f.doc == SchemaDoc.Ref("RecNode")))
      ,
      test("a list on the cycle is an array of that ref"):
        val xs = field(summon[Schema[RecList]].doc, "xs")
        assertTrue(xs.exists(f => !f.optional && f.doc == SchemaDoc.Array(SchemaDoc.Ref("RecList"))))
      ,
      test("a chunk on the cycle is an array of that ref"):
        val xs = field(summon[Schema[RecChunk]].doc, "xs")
        assertTrue(xs.exists(f => !f.optional && f.doc == SchemaDoc.Array(SchemaDoc.Ref("RecChunk"))))
      ,
      test("an option on the cycle stays optional"):
        val item = field(summon[Schema[RecOpt]].doc, "item")
        assertTrue(item.exists(f => f.optional && f.doc == SchemaDoc.Ref("RecOpt")))
      ,
      test("an either on the cycle keeps both sides"):
        val choice = field(summon[Schema[RecChoice]].doc, "choice")
        assertTrue(
          choice.exists(f =>
            !f.optional && f.doc == SchemaDoc.OneOf(None, List(SchemaDoc.Ref("RecChoice"), SchemaDoc.Str(None)))
          )
        )
      ,
      test("mutual recursion points each field at the other component"):
        val toB = field(summon[Schema[RecA]].doc, "b")
        val toA = field(summon[Schema[RecB]].doc, "a")
        assertTrue(
          toB.exists(f => !f.optional && f.doc == SchemaDoc.Ref("RecB")),
          toA.exists(f => !f.optional && f.doc == SchemaDoc.Ref("RecA")),
        )
      ,
      test("a later product inlines the finished cyclic schema"):
        val kids   = summon[Schema[RecKids]].doc
        val packed = summon[Schema[RecPacked]].doc
        val maybe  = summon[Schema[RecMaybe]].doc
        val xs     = field(kids, "xs")
        val chunk  = field(packed, "xs")
        val item   = field(maybe, "item")
        assertTrue(
          xs.exists(f => !f.optional && titled(f.doc).contains("RecNode")),
          chunk.exists(f => !f.optional && titled(f.doc).contains("RecNode")),
          item.exists(f => f.optional && titled(f.doc).contains("RecNode")),
          refs(kids).contains("RecNode"),
        )
      ,
      test("a list summoned after derivation sees the finished schema"):
        summon[Schema[List[RecList]]].doc match
          case SchemaDoc.Array(SchemaDoc.OneOf(Some("RecList"), _)) => assertTrue(true)
          case _                                                    => assertTrue(false)
      ,
      test("a summoned schema is kept"):
        summon[Schema[Ticket]].doc match
          case SchemaDoc.Object(Some("Ticket"), List(SchemaField("tag", SchemaDoc.Str(Some("tag")), false)), _) =>
            assertTrue(true)
          case _ =>
            assertTrue(false),
    ) @@ TestAspect.timeout(5.seconds)

  private def field(doc: SchemaDoc, name: String): Option[SchemaField] =
    doc match
      case SchemaDoc.Object(_, fields, _) =>
        fields.collectFirst { case f if f.name == name => f }.orElse {
          fields.iterator.map(f => field(f.doc, name)).collectFirst { case Some(found) => found }
        }
      case SchemaDoc.OneOf(_, variants) =>
        variants.iterator.map(field(_, name)).collectFirst { case Some(found) => found }
      case SchemaDoc.Optional(inner) => field(inner, name)
      case SchemaDoc.Array(items)    => field(items, name)
      case _                         => None

  private def titled(doc: SchemaDoc): Option[String] =
    doc match
      case SchemaDoc.Array(items)        => titled(items)
      case SchemaDoc.Optional(inner)     => titled(inner)
      case SchemaDoc.Object(title, _, _) => title
      case SchemaDoc.OneOf(title, _)     => title
      case SchemaDoc.Enum(title, _)      => title
      case _                             => None

  private def refs(doc: SchemaDoc): List[String] =
    doc match
      case SchemaDoc.Ref(name)            => List(name)
      case SchemaDoc.Array(items)         => refs(items)
      case SchemaDoc.Optional(inner)      => refs(inner)
      case SchemaDoc.Object(_, fields, _) => fields.flatMap(f => refs(f.doc))
      case SchemaDoc.OneOf(_, variants)   => variants.flatMap(refs)
      case _                              => Nil

  private def wrapperLabel(doc: SchemaDoc): Option[String] =
    doc match
      case SchemaDoc.Object(None, List(field), List(label)) if field.name == label => Some(label)
      case _                                                                       => None

  sealed trait Outer derives Schema
  sealed trait Middle                  extends Outer
  final case class A(n: Int)           extends Middle
  final case class B(s: String)        extends Middle
  final case class Leaf(flag: Boolean) extends Outer

  sealed trait Dir derives Schema
  sealed trait Vertical extends Dir
  case object North     extends Vertical
  case object South     extends Vertical
  case object East      extends Dir

  enum RecNode derives Schema:
    case Tip
    case Branch(child: RecNode)

  enum RecList derives Schema:
    case Tip
    case Branch(xs: List[RecList])

  enum RecChunk derives Schema:
    case Tip
    case Branch(xs: Chunk[RecChunk])

  enum RecOpt derives Schema:
    case Tip
    case Branch(item: Option[RecOpt])

  enum RecChoice derives Schema:
    case Tip
    case Branch(choice: Either[RecChoice, String])

  enum RecA derives Schema:
    case ToB(b: RecB)

  enum RecB derives Schema:
    case ToA(a: RecA)

  final case class RecKids(xs: List[RecNode]) derives Schema
  final case class RecPacked(xs: Chunk[RecNode]) derives Schema
  final case class RecMaybe(item: Option[RecNode]) derives Schema

  final case class Tag(value: String)
  given Schema[Tag] = Schema.of(SchemaDoc.Str(Some("tag")), Some("Tag"))
  final case class Ticket(tag: Tag) derives Schema
end SchemaSpec
