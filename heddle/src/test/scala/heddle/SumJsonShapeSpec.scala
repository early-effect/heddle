package heddle

import zio.json.*
import zio.test.*

object SumJsonShapeSpec extends ZIOSpecDefault:
  enum Mixed derives JsonCodec:
    case Payload(id: Int)
    case Bare

  sealed trait Traited derives JsonCodec
  object Traited:
    final case class Payload(id: Int) extends Traited
    case object Bare                  extends Traited

  enum Flat derives JsonCodec:
    case Up
    case Down

  sealed trait Outer derives JsonCodec
  sealed trait Middle                  extends Outer
  final case class Inner(n: Int)       extends Middle
  final case class Leaf(flag: Boolean) extends Outer

  sealed trait Dir derives JsonCodec
  sealed trait Vertical extends Dir
  case object North     extends Vertical
  case object East      extends Dir

  def spec =
    suite("zio-json sum shapes")(
      test("a payload case is wrapped in its label"):
        assertTrue((Mixed.Payload(1): Mixed).toJson == """{"Payload":{"id":1}}""")
      ,
      test("a singleton in a mixed enum is an empty wrapped object"):
        assertTrue((Mixed.Bare: Mixed).toJson == """{"Bare":{}}""")
      ,
      test("a sealed trait case object is an empty wrapped object"):
        assertTrue(
          (Traited.Payload(1): Traited).toJson == """{"Payload":{"id":1}}""",
          (Traited.Bare: Traited).toJson == """{"Bare":{}}""",
        )
      ,
      test("an all-singleton enum is a plain string"):
        assertTrue((Flat.Up: Flat).toJson == "\"Up\"", "\"Down\"".fromJson[Flat] == Right(Flat.Down))
      ,
      test("nested sealed hierarchies wrap by leaf label"):
        assertTrue((Inner(1): Outer).toJson == """{"Inner":{"n":1}}""")
      ,
      test("nested all-singleton hierarchies are plain leaf strings"):
        assertTrue((North: Dir).toJson == "\"North\"", (East: Dir).toJson == "\"East\""),
    )
end SumJsonShapeSpec
