package heddle.datastar

import heddle.*
import heddle.sse.{ServerSentEvent, Sse}
import zio.*

enum ElementPatchMode:
  case Outer, Inner, Replace, Append, Prepend, Before, After, Remove
  def wire: String = productPrefix.toLowerCase

final case class PatchElementOptions(
    selector: Option[String] = None,
    mode: ElementPatchMode = ElementPatchMode.Outer,
)

trait Datastar:
  def send(event: ServerSentEvent): UIO[Unit]

object Datastar:
  def send(event: ServerSentEvent): URIO[Datastar, Unit] =
    ZIO.serviceWithZIO(_.send(event))

  def events[R, E <: Throwable](run: ZIO[R & Datastar & Scope, E, Unit]): Handler[R, Nothing] =
    Handler { _ =>
      Sse.session[R] { w =>
        val ds: Datastar = (event: ServerSentEvent) => w.send(event)
        run.provideSomeLayer[R & Scope](ZLayer.succeed(ds))
      }
    }

  def events[R, E <: Throwable](h: Handler[R & Datastar & Scope, E]): Handler[R, Nothing] =
    Handler { req =>
      Sse.session[R] { w =>
        val ds: Datastar = (event: ServerSentEvent) => w.send(event)
        h.run(req).provideSomeLayer[R & Scope](ZLayer.succeed(ds)).unit
      }
    }
end Datastar

def events[R, E <: Throwable](run: ZIO[R & Datastar & Scope, E, Unit]): Handler[R, Nothing] =
  Datastar.events(run)

def events[R, E <: Throwable](h: Handler[R & Datastar & Scope, E]): Handler[R, Nothing] =
  Datastar.events(h)

extension (req: Request)
  def readSignals[A](using codec: JsonCodec[A]): Task[A] =
    req.body.utf8.flatMap { json =>
      ZIO.fromEither(codec.decode(json)).mapError(msg => IllegalArgumentException(msg))
    }

object ServerSentEventGenerator:
  def patchElements(html: String, options: PatchElementOptions = PatchElementOptions()): URIO[Datastar, Unit] =
    Datastar.send(encodeElements(html, options))

  def patchSignals(json: String): URIO[Datastar, Unit] =
    Datastar.send(encodeSignals(json))

  private def encodeElements(html: String, opt: PatchElementOptions): ServerSentEvent =
    val lines = List.newBuilder[String]
    opt.selector.foreach(s => lines += s"selector $s")
    if opt.mode != ElementPatchMode.Outer then lines += s"mode ${opt.mode.wire}"
    html.split("\n", -1).foreach(line => lines += s"elements $line")
    ServerSentEvent(lines.result().mkString("\n"), Some("datastar-patch-elements"))

  private def encodeSignals(json: String): ServerSentEvent =
    ServerSentEvent(s"signals $json", Some("datastar-patch-signals"))
end ServerSentEventGenerator
