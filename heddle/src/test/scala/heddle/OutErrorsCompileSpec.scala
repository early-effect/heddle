package heddle

import scala.compiletime.testing.typeCheckErrors
import zio.test.*

object OutErrorsCompileSpec extends ZIOSpecDefault:
  private def mentions(errors: List[scala.compiletime.testing.Error], text: String): TestResult =
    assertTrue(errors.exists(_.message.contains(text)))

  def spec =
    suite("outErrors compile-time checks")(
      test("a missing case is named"):
        mentions(
          typeCheckErrors("""
            Endpoint.get("x").outErrors[OrderError](
              ErrorCase[OrderError.NotFound](Status.NotFound),
              ErrorCase[OrderError.Conflict](Status.Conflict),
            )
          """),
          "outErrors[heddle.OrderError] is missing ErrorCase for: Unavailable",
        )
      ,
      test("a case listed twice is named"):
        mentions(
          typeCheckErrors("""
            Endpoint.get("x").outErrors[OrderError](
              ErrorCase[OrderError.NotFound](Status.NotFound),
              ErrorCase[OrderError.Conflict](Status.Conflict),
              ErrorCase[OrderError.NotFound](Status.Gone),
              ErrorCase[OrderError.Unavailable.type](Status.ServiceUnavailable),
            )
          """),
          "NotFound is listed twice in outErrors",
        )
      ,
      test("a leaf below a nested sealed trait is not a direct case"):
        mentions(
          typeCheckErrors("""
            Endpoint.get("x").outErrors[Lookup](
              ErrorCase[Lookup.NoUser](Status.NotFound),
              ErrorCase[Lookup.Throttled.type](Status.TooManyRequests),
            )
          """),
          "is not a case of heddle.Lookup. Cases: Missing, Throttled",
        )
      ,
      test("a type outside the error ADT is rejected by the pinned E"):
        assertTrue(
          typeCheckErrors("""
            Endpoint.get("x").outErrors[OrderError](ErrorCase[String](Status.NotFound))
          """).nonEmpty
        )
      ,
      test("an error type that is not a sum is rejected"):
        mentions(
          typeCheckErrors("""Endpoint.get("x").outErrors[String](ErrorCase[String](Status.NotFound))"""),
          "outErrors needs a sealed trait or enum",
        )
      ,
      test("a splatted Seq is rejected"):
        mentions(
          typeCheckErrors("""
            val cs = Seq(ErrorCase[Light.Red.type](Status.Forbidden), ErrorCase[Light.Green.type](Status.Gone))
            Endpoint.get("x").outErrors[Light](cs*)
          """),
          "outErrors takes ErrorCase values listed inline",
        )
      ,
      test("an ErrorCase with an existential case type is rejected"):
        mentions(
          typeCheckErrors("""
            val red: ErrorCase[? <: Light] = ErrorCase[Light.Red.type](Status.Forbidden)
            Endpoint.get("x").outErrors[Light](red, ErrorCase[Light.Green.type](Status.Gone))
          """),
          "ErrorCase needs a concrete case of heddle.Light",
        ),
    )
end OutErrorsCompileSpec
