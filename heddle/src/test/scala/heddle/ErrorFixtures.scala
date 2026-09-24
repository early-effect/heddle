package heddle

sealed trait OrderError derives Schema, JsonCodec
object OrderError:
  final case class NotFound(id: Int)        extends OrderError
  final case class Conflict(reason: String) extends OrderError
  case object Unavailable                   extends OrderError

  val all: List[OrderError] = List(NotFound(1), Conflict("busy"), Unavailable)

enum Seating derives Schema, JsonCodec:
  case SoldOut(showId: Int)
  case NoBlock(showId: Int, size: Int)
  case Closed

enum Light derives Schema, JsonCodec:
  case Red
  case Green

sealed trait Lookup derives Schema, JsonCodec
object Lookup:
  sealed trait Missing             extends Lookup
  final case class NoUser(id: Int) extends Missing
  final case class NoOrg(id: Int)  extends Missing
  case object Throttled            extends Lookup

object ErrorFixtures:
  val order =
    Endpoint
      .post("orders" / int("id"))
      .out[String]
      .outErrors[OrderError](
        ErrorCase[OrderError.NotFound](Status.NotFound),
        ErrorCase[OrderError.Conflict](Status.Conflict),
        ErrorCase[OrderError.Unavailable.type](Status.ServiceUnavailable),
      )

  val seating =
    Endpoint
      .post("seats")
      .out[String]
      .outErrors[Seating](
        ErrorCase[Seating.SoldOut](Status.Conflict),
        ErrorCase[Seating.NoBlock](Status.Conflict),
        ErrorCase[Seating.Closed.type](Status.Gone),
      )

  val light =
    Endpoint
      .get("light")
      .out[String]
      .outErrors[Light](
        ErrorCase[Light.Red.type](Status.Forbidden),
        ErrorCase[Light.Green.type](Status.Gone),
      )

  val lookup =
    Endpoint
      .get("lookup")
      .out[String]
      .outErrors[Lookup](
        ErrorCase[Lookup.Missing](Status.NotFound),
        ErrorCase[Lookup.Throttled.type](Status.TooManyRequests),
      )
end ErrorFixtures
