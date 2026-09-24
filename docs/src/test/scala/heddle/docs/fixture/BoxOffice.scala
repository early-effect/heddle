package heddle.docs.fixture

import heddle.*
import heddle.mcp.Mcp
import heddle.mcp.protocol.JsonRpc.*
import zio.*
import zio.json.JsonCodec
import zio.json.ast.Json

final case class Show(id: Int, title: String, remaining: Int) derives Schema, JsonCodec
final case class Party(showId: Int, size: Int) derives Schema, JsonCodec
final case class PartySeated(
    orderId: Int,
    seats: List[String],
    totalCents: Int,
    pickupCode: String,
) derives Schema,
      JsonCodec
final case class NewHold(showId: Int, size: Int) derives Schema, JsonCodec
final case class Hold(id: Int, showId: Int, seats: List[String]) derives Schema, JsonCodec
final case class ShowNotFound(message: String) derives Schema, JsonCodec
enum SeatingError derives Schema, JsonCodec:
  case SoldOut(showId: Int)
  case NoBlock(showId: Int, size: Int)

object BoxOffice:
  val listShows =
    Endpoint.get("shows").out[List[Show]].name("list_shows").summary("What is on").hints(Hint.ReadOnly)

  val getShow =
    Endpoint
      .get("shows" / int("id"))
      .out[Show]
      .outError[ShowNotFound](Status.NotFound)
      .name("get_show")
      .summary("One bill")
      .hints(Hint.ReadOnly)

  val createHold =
    Endpoint
      .post("holds")
      .inJson[NewHold]
      .out[Hold](Status.Created)
      .outErrors[SeatingError](
        ErrorCase[SeatingError.SoldOut](Status.Conflict),
        ErrorCase[SeatingError.NoBlock](Status.UnprocessableContent),
      )
      .name("create_hold")

  val seatTheParty =
    Endpoint
      .post("parties")
      .inJson[Party]
      .out[PartySeated](Status.Created)
      .outErrors[SeatingError](
        ErrorCase[SeatingError.SoldOut](Status.Conflict),
        ErrorCase[SeatingError.NoBlock](Status.UnprocessableContent),
      )
      .name("seat_the_party")
      .summary("Hold a contiguous block, price it, return a pickup code")
      .hints(Hint.Destructive)

  def seed: UIO[Ref[Map[Int, Show]]] =
    Ref.make(Map(1 -> Show(1, "Evening bill", 12), 2 -> Show(2, "Matinee", 8), 3 -> Show(3, "Late bill", 2)))

  def api(store: Ref[Map[Int, Show]]): Api[Any] =
    Api("Box office", "0.1.0")
      .job(listShows)(_ => store.get.map(_.values.toList.sortBy(_.id)))
      .job(getShow) { id =>
        store.get.flatMap(m => ZIO.fromOption(m.get(id)).orElseFail(ShowNotFound(s"show $id")))
      }
      .resource(createHold)(_ => ZIO.fail(SeatingError.SoldOut(0)))
      .job(seatTheParty) { party =>
        for
          show <- store.get
            .flatMap(m => ZIO.fromOption(m.get(party.showId)).orElseFail(SeatingError.SoldOut(party.showId)))
          _ <- ZIO.fail(SeatingError.SoldOut(party.showId)).when(show.remaining < party.size)
          _ <- ZIO.fail(SeatingError.NoBlock(party.showId, party.size)).when(party.showId == 3 && party.size == 2)
          (total, code) <- ZIO.succeed(party.size * 2500).zipPar(ZIO.succeed("P1001"))
        yield PartySeated(1, List("A1", "A2").take(party.size), total, code)
      }

  def mcpOf(store: Ref[Map[Int, Show]]): Either[String, Mcp[Any]] =
    Mcp.from(api(store))

  def rpc(method: String, params: Json.Obj, id: Int = 1): Json.Obj =
    val meta = obj(MetaVersion -> Json.Str(ProtocolVersion), MetaClientCaps -> obj())
    val p    = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> p)
end BoxOffice
