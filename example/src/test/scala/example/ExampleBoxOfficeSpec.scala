package example

import zio.*
import zio.test.*

object ExampleBoxOfficeSpec extends ZIOSpecDefault:
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
        yield assertTrue(miss == Left(SeatingError.NoBlock(3, 2))),
    ) @@ TestAspect.timeout(5.seconds)
end ExampleBoxOfficeSpec
