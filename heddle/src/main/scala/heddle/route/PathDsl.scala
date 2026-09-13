package heddle.route

import heddle.http.Method

object PathDsl:
  extension (inline method: Method)
    inline def /(inline lit: String): RoutePattern[Unit] =
      RoutePattern(method, PathCodec.specialize(PathCodec.lit(lit)))
    inline def /[A](inline codec: PathCodec[A]): RoutePattern[A] =
      RoutePattern(method, PathCodec.specialize(codec))

  extension (inline literal: String)
    inline def /(inline next: String): PathCodec[Unit]        = PathCodec.lit(literal) / next
    inline def /[A](inline codec: PathCodec[A]): PathCodec[A] = PathCodec.lit(literal) / codec
end PathDsl
