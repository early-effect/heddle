package heddle.server

import zio.Config

/** Load a neotype from `zio.Config`. A missing key is the default. A present value that `make` rejects is
  * `InvalidData`, not the default.
  */
private[heddle] object Configured:
  def int[A](key: String, default: A, raw: A => Int, make: Int => Either[String, A]): Config[A] =
    Config
      .int(key)
      .withDefault(raw(default))
      .mapOrFail(n => make(n).left.map(message => Config.Error.InvalidData(message = message)))

  def long[A](key: String, default: A, raw: A => Long, make: Long => Either[String, A]): Config[A] =
    Config
      .long(key)
      .withDefault(raw(default))
      .mapOrFail(n => make(n).left.map(message => Config.Error.InvalidData(message = message)))

  def optionalInt[A](key: String, make: Int => Either[String, A]): Config[Option[A]] =
    Config.int(key).optional.mapOrFail {
      case None    => Right(None)
      case Some(n) => make(n).left.map(message => Config.Error.InvalidData(message = message)).map(Some(_))
    }
end Configured
