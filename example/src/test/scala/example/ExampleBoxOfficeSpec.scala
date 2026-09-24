package example

import heddle.*
import zio.*
import zio.json.EncoderOps
import zio.test.*

object ExampleBoxOfficeSpec extends ZIOSpecDefault:
  private def seat(office: BoxOffice, party: Party) =
    Main.writeApi(office).routes(Request.post("/parties", Body.json(party.toJson)))

  def spec =
    suite("box office")(
      test("seat a party of 2 returns seats and a pickup code"):
        for
          office <- BoxOffice.seed
          seated <- office.seat(Party(1, 2))
        yield assertTrue(seated.seats == List("A1", "A2"), seated.pickupCode == "P1001", seated.totalCents == 5000)
      ,
      test("SoldOut when the bill cannot take the party"):
        for
          office <- BoxOffice.seed
          miss   <- office.seat(Party(1, 13)).either
        yield assertTrue(miss == Left(SeatingError.SoldOut(1)))
      ,
      test("NoBlock when remaining seats are not contiguous"):
        for
          office <- BoxOffice.seed
          miss   <- office.seat(Party(3, 2)).either
        yield assertTrue(miss == Left(SeatingError.NoBlock(3, 2)))
      ,
      test("over HTTP SoldOut answers 409 and NoBlock answers 422"):
        for
          office  <- BoxOffice.seed
          soldOut <- seat(office, Party(1, 13))
          noBlock <- seat(office, Party(3, 2))
        yield assertTrue(
          soldOut.status == Status.Conflict,
          soldOut.body.asString == """{"SoldOut":{"showId":1}}""",
          noBlock.status == Status.UnprocessableContent,
          noBlock.body.asString == """{"NoBlock":{"showId":3,"size":2}}""",
        ),
    ) @@ TestAspect.timeout(5.seconds)
end ExampleBoxOfficeSpec
