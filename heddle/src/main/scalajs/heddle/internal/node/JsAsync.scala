package heddle.internal.node

import scala.scalajs.js
import zio.{Task, ZIO}

/** `js.Promise` into ZIO. Explicit at every call site; there is no implicit conversion. */
private[heddle] object JsAsync:
  /** `promise` is by name: each run of the effect starts it again, so a repeated `reader.read()` reads onward. */
  def fromPromise[A](promise: => js.Promise[A]): Task[A] =
    ZIO.async[Any, Throwable, A] { register =>
      val onOk: js.Function1[A, Unit] =
        (a: A) => register(ZIO.succeed(a))
      val onErr: js.Function1[Any, Unit] =
        (err: Any) => register(ZIO.fail(toThrowable(err)))
      val _ = promise.`then`[Unit](onOk, onErr)
      ()
    }

  private def toThrowable(err: Any): Throwable =
    err match
      case t: Throwable => t
      case e: js.Error  => java.io.IOException(Option(e.message).getOrElse(e.toString))
      case other        => java.io.IOException(String.valueOf(other))
end JsAsync
