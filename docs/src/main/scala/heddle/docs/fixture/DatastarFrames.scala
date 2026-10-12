package heddle.docs.fixture

import heddle.*
import heddle.datastar.*
import heddle.docs.ui.ScalaLiteral
import zio.*

/** The Datastar pictures, taken from the SSE body `ServerSentEventGenerator` actually writes. */
object DatastarFrames:

  final case class Picture(elements: String, signals: String)

  enum Gap:
    case NotServed(status: String)
    case EmptyFrame
    case BodyFailed(kind: String)

  /** The same two sends the Realtime page shows, each as its own response body. */
  def load: IO[Gap, Picture] =
    for
      elements <- frame(
        ServerSentEventGenerator.patchElements(
          "<div>hello</div>",
          PatchElementOptions(selector = Some("#app"), mode = ElementPatchMode.Inner),
        )
      )
      signals <- frame(ServerSentEventGenerator.patchSignals("""{"count":1}"""))
    yield Picture(elements, signals)

  /** Second object in the generated transcript file. The package clause is already written. */
  def source(picture: Picture): String =
    "object DatastarTranscript:\n" +
      "  /** Generated from ServerSentEventGenerator by WriteTranscript. Do not edit. */\n" +
      "  val elements: String =\n    " + ScalaLiteral(picture.elements) + "\n" +
      "  val signals: String =\n    " + ScalaLiteral(picture.signals) + "\n"

  private def frame(send: URIO[Datastar, Unit]): IO[Gap, String] =
    Routes(Method.GET / "sse" -> events(send)).runZIO(Request.get("/sse")).flatMap { res =>
      if res.status.isSuccess then
        res.body.utf8.mapError(t => Gap.BodyFailed(t.getClass.getName)).flatMap { raw =>
          if raw.isEmpty then ZIO.fail(Gap.EmptyFrame) else ZIO.succeed(raw)
        }
      else ZIO.fail(Gap.NotServed(res.status.render))
    }
end DatastarFrames
