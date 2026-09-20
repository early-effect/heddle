package heddle.route

import heddle.http.Path
import java.util.UUID
import zio.Chunk

type Combine[A, B] = A match
  case Unit => B
  case _    =>
    B match
      case Unit => A
      case _    => (A, B)

object Combine:
  inline def apply[A, B](a: A, b: B): Combine[A, B] =
    (a, b) match
      case ((), bVal)   => bVal.asInstanceOf[Combine[A, B]]
      case (aVal, ())   => aVal.asInstanceOf[Combine[A, B]]
      case (aVal, bVal) => (aVal, bVal).asInstanceOf[Combine[A, B]]

  inline def unapply[A, B](c: Combine[A, B]): (A, B) =
    inline scala.compiletime.erasedValue[A] match
      case _: Unit => (().asInstanceOf[A], c.asInstanceOf[B])
      case _       =>
        inline scala.compiletime.erasedValue[B] match
          case _: Unit => (c.asInstanceOf[A], ().asInstanceOf[B])
          case _       => c.asInstanceOf[(A, B)]
end Combine

enum PathKind:
  case Int32, Int64, Str, Uuid

  def openApiType: String =
    this match
      case Int32 | Int64 => "integer"
      case Str | Uuid    => "string"

  def openApiFormat: Option[String] =
    this match
      case Int32 => Some("int32")
      case Int64 => Some("int64")
      case Uuid  => Some("uuid")
      case Str   => None
end PathKind

enum Seg:
  case Lit(value: String)
  case Var(name: String, kind: PathKind)
  case Rest

final class PathCodec[A](
    val segments: Chunk[Seg],
    private[heddle] val extractFn: Chunk[String] => Option[A],
    private[heddle] val specialized: Boolean = false,
):
  private val len: Int = segments.length

  private[heddle] val isLiteral: Boolean =
    var i    = 0
    var vars = false
    while i < len && !vars do
      segments(i) match
        case Seg.Var(_, _) | Seg.Rest => vars = true
        case _                        => ()
      i += 1
    !vars

  def matchPath(parts: List[String]): Option[(A, List[String])] =
    if parts.length < len then None
    else
      val (head, rest) = parts.splitAt(len)
      extractFn(Chunk.fromIterable(head)).map(a => (a, rest))

  def matches(path: Path): Option[A] =
    extractFn(path.segments)

  def encode(value: A): Path =
    PathCodec.encodePath(segments, value)

  def /(lit: String): PathCodec[A] =
    PathLits.register(lit)
    PathCodec.of(segments :+ Seg.Lit(lit))

  def /[B](that: PathCodec[B]): PathCodec[Combine[A, B]] =
    PathCodec.of(segments ++ that.segments)

  def template: String =
    if segments.isEmpty then "/"
    else
      segments
        .map {
          case Seg.Lit(v)       => v
          case Seg.Var(name, _) => s"{$name}"
          case Seg.Rest         => "*"
        }
        .mkString("/", "/", "")

  def pathParams: Chunk[(String, PathKind)] =
    segments.collect { case Seg.Var(name, kind) => (name, kind) }
end PathCodec

object PathCodec:
  val empty: PathCodec[Unit] = of(Chunk.empty)

  /** Remaining path segments, including none (`/`). */
  val trailing: PathCodec[Path] =
    PathCodec(Chunk(Seg.Rest), segs => Some(Path(segs)))

  def of[A](segments: Chunk[Seg]): PathCodec[A] =
    val steps = segments.toArray
    PathCodec(segments, segs => extract(steps, segs))

  inline def specialize[A](inline path: PathCodec[A]): PathCodec[A] =
    ${ PathMacros.specializeImpl[A]('path) }

  def lit(value: String): PathCodec[Unit] =
    PathLits.register(value)
    of(Chunk(Seg.Lit(value)))

  def int(name: String): PathCodec[Int] =
    of(Chunk(Seg.Var(name, PathKind.Int32)))

  def long(name: String): PathCodec[Long] =
    of(Chunk(Seg.Var(name, PathKind.Int64)))

  def string(name: String): PathCodec[String] =
    of(Chunk(Seg.Var(name, PathKind.Str)))

  def uuid(name: String): PathCodec[UUID] =
    of(Chunk(Seg.Var(name, PathKind.Uuid)))

  private[heddle] def extract[A](steps: Array[Seg], segs: Chunk[String]): Option[A] =
    val n        = steps.length
    var i        = 0
    var si       = 0
    var acc: Any = ()
    var hasRest  = false
    while i < n do
      steps(i) match
        case Seg.Lit(value) =>
          if si >= segs.length || segs(si) != value then return None
          si += 1
        case Seg.Var(_, kind) =>
          if si >= segs.length then return None
          val parsed: Option[Any] = kind match
            case PathKind.Int32 => segs(si).toIntOption
            case PathKind.Int64 => segs(si).toLongOption
            case PathKind.Str   => Some(segs(si))
            case PathKind.Uuid  =>
              try Some(UUID.fromString(segs(si)))
              catch case _: IllegalArgumentException => None
          parsed match
            case None    => return None
            case Some(v) => acc = Combine(acc, v)
          si += 1
        case Seg.Rest =>
          acc = Combine(acc, Path(segs.drop(si)))
          si = segs.length
          hasRest = true
      end match
      i += 1
    end while
    if !hasRest && si != segs.length then None
    else Some(acc.asInstanceOf[A])
  end extract

  private[heddle] def encodePath[A](segments: Chunk[Seg], value: A): Path =
    val steps = segments.toArray
    val out   = Array.newBuilder[String]
    var vals  = flatten(value)
    var i     = 0
    while i < steps.length do
      steps(i) match
        case Seg.Lit(v)    => out += v
        case Seg.Var(_, _) =>
          vals match
            case h :: t =>
              out += renderVal(h)
              vals = t
            case Nil => ()
        case Seg.Rest =>
          vals match
            case (p: Path) :: t =>
              p.segments.foreach(s => out += s)
              vals = t
            case h :: t =>
              out += renderVal(h)
              vals = t
            case Nil => ()
      end match
      i += 1
    end while
    Path(Chunk.fromIterable(out.result()))
  end encodePath

  private def flatten(a: Any): List[Any] =
    a match
      case ()              => Nil
      case t: Tuple2[?, ?] => flatten(t._1) ++ flatten(t._2)
      case other           => other :: Nil

  private def renderVal(v: Any): String =
    v match
      case u: UUID => u.toString
      case other   => other.toString
end PathCodec

inline def int(inline name: String): PathCodec[Int]       = PathCodec.int(name)
inline def long(inline name: String): PathCodec[Long]     = PathCodec.long(name)
inline def string(inline name: String): PathCodec[String] = PathCodec.string(name)
inline def uuid(inline name: String): PathCodec[UUID]     = PathCodec.uuid(name)
val trailing: PathCodec[Path]                             = PathCodec.trailing
