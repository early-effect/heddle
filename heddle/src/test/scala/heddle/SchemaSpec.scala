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
          case _ => assertTrue(false),
    ) @@ TestAspect.timeout(5.seconds)
end SchemaSpec
