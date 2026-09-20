package heddle.internal.node

import scala.scalajs.js
import zio.{Task, ZIO}

/** `js.Promise` into ZIO.
  *
  * The [[Conversion]] is `private[heddle]` and lives behind `import JsAsync.given`. It is not on `import heddle.*`.
  */
private[heddle] object JsAsync:
  def fromPromise[A](promise: js.Promise[A]): Task[A] =
    ZIO.async[Any, Throwable, A] { register =>
      val onOk: js.Function1[A, Unit] =
        (a: A) => register(ZIO.succeed(a))
      val onErr: js.Function1[Any, Unit] =
        (err: Any) => register(ZIO.fail(toThrowable(err)))
      val _ = promise.`then`[Unit](onOk, onErr)
      ()
    }

  given jsPromiseToTask[A]: Conversion[js.Promise[A], Task[A]] =
    fromPromise(_)

  private def toThrowable(err: Any): Throwable =
    err match
      case t: Throwable => t
      case e: js.Error  => java.io.IOException(Option(e.message).getOrElse(e.toString))
      case other        => java.io.IOException(String.valueOf(other))
end JsAsync
