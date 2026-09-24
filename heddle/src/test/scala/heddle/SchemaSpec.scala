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
        assertTrue(summon[Schema[Dir]].doc == SchemaDoc.Enum(Some("Dir"), List("North", "South", "East"))),
    ) @@ TestAspect.timeout(5.seconds)

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
end SchemaSpec
