package heddle

import zio.test.*

object StatusSpec extends ZIOSpecDefault:
  def spec =
    suite("Status")(
      test("fromCode uses RFC phrases for catalogued codes"):
        assertTrue(
          Status.fromCode(206) == Status.PartialContent,
          Status.fromCode(413) == Status.ContentTooLarge,
          Status.fromCode(422) == Status.UnprocessableContent,
        )
      ,
      test("fromCode synthesizes Unknown off catalog"):
        assertTrue(Status.fromCode(599) == Status(599, "Unknown")),
    )
end StatusSpec
