package heddle.route

import heddle.http.Method

object PathDsl:
  extension (inline method: Method)
    transparent inline def /(inline lit: String) =
      RoutePattern(method, PathCodec.specialize(PathCodec.lit(lit)))
    transparent inline def /[N <: Tuple, A](inline codec: PathCodec[A] { type Names = N }) =
      RoutePattern(method, PathCodec.specialize(codec))

  extension (inline literal: String)
    inline def /(inline next: String): PathCodec[Unit] { type Names = EmptyTuple } =
      PathCodec.lit(literal).appendLit[EmptyTuple](next)
    transparent inline def /[N <: Tuple, A](inline codec: PathCodec[A] { type Names = N }) =
      codec.prefixed[N](literal)

  extension [N <: Tuple, A](self: PathCodec[A] { type Names = N })
    /** A literal after this path. Captures and their names stay. */
    inline def /(inline lit: String): PathCodec[A] { type Names = N } =
      self.appendLit[N](lit)

    /** This path, then `that`. The same capture name on both sides does not compile. */
    transparent inline def /[M <: Tuple, B](that: PathCodec[B] { type Names = M })(using
        c: Combiner[A, B]
    ): PathCodec[c.Out] { type Names = Tuple.Concat[N, M] } =
      NamesCheck.distinct[N, M]
      self.joined[N, M, B](that)
  end extension
end PathDsl
