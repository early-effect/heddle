package heddle.route

/** How two inputs become one value, and back. `Unit` disappears, a value joins a tuple at its end, and anything else
  * pairs: `int("id") / string("slug") / long("n")` is `(Int, String, Long)`.
  */
trait Combiner[A, B]:
  type Out
  def combine(a: A, b: B): Out
  def separate(out: Out): (A, B)

object Combiner extends CombinerRightUnit:
  type WithOut[A, B, O] = Combiner[A, B] { type Out = O }

  given leftUnit[B]: WithOut[Unit, B, B] = new Combiner[Unit, B]:
    type Out = B
    def combine(a: Unit, b: B): B   = b
    def separate(out: B): (Unit, B) = ((), out)

trait CombinerRightUnit extends CombinerAppend:
  given rightUnit[A]: Combiner.WithOut[A, Unit, A] = new Combiner[A, Unit]:
    type Out = A
    def combine(a: A, b: Unit): A   = a
    def separate(out: A): (A, Unit) = (out, ())

trait CombinerAppend extends CombinerPair:
  /** `(a1, ..., an)` and `b` is `(a1, ..., an, b)`, generated per arity where it is used. */
  transparent inline given append[T <: NonEmptyTuple, B]: Combiner[T, B] = ${ CombinerMacros.append[T, B] }

trait CombinerPair:
  given pair[A, B]: Combiner.WithOut[A, B, (A, B)] = new Combiner[A, B]:
    type Out = (A, B)
    def combine(a: A, b: B): (A, B)   = (a, b)
    def separate(out: (A, B)): (A, B) = out
