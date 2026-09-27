package heddle.route

import heddle.http.Path
import java.util.UUID
import zio.Chunk

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

/** A typed path. `segments` route and document it; `read` takes its value from a request path, and `write` puts one
  * back, so a typed client builds the path a server matches.
  */
final class PathCodec[A] private[heddle] (
    val segments: Chunk[Seg],
    private[heddle] val read: PathCodec.Reader[A],
    private[heddle] val write: A => Chunk[String],
    fast: Option[Chunk[String] => Option[A]] = None,
):
  /** How many request segments this path reads, or `None` when it ends in `trailing` and takes the rest. */
  private[heddle] val width: Option[Int] =
    if segments.contains(Seg.Rest) then None else Some(segments.length)

  private val fixed: Int = segments.count(_ != Seg.Rest)

  private val literalAt: Array[Int]       = segments.zipWithIndex.collect { case (Seg.Lit(_), i) => i }.toArray
  private val literalValue: Array[String] = segments.collect { case Seg.Lit(v) => v }.toArray

  private def literalsMatch(path: Chunk[String]): Boolean =
    var i = 0
    while i < literalAt.length && path(literalAt(i)) == literalValue(i) do i += 1
    i == literalAt.length

  /** True when `PathCodec.specialize` unrolled this path into a straight-line matcher. */
  private[heddle] val specialized: Boolean = fast.isDefined

  private[heddle] val isLiteral: Boolean =
    segments.forall {
      case Seg.Lit(_) => true
      case _          => false
    }

  /** A path with no captures matches exactly one request path, so its value is computed once. */
  private[heddle] val literalMatch: Option[A] =
    if isLiteral then matches(Path(segments.collect { case Seg.Lit(v) => v })) else None

  def matches(path: Path): Option[A] =
    fast match
      case Some(unrolled) => unrolled(path.segments)
      case None           =>
        val n = path.segments.length
        if width.fold(n < fixed)(_ != n) || !literalsMatch(path.segments) then None else read(path.segments, 0)

  def encode(value: A): Path =
    Path(write(value))

  /** A literal after this path: checked with the others, so the value reader is unchanged. */
  def /(lit: String): PathCodec[A] =
    PathLits.register(lit)
    PathCodec(segments :+ Seg.Lit(lit), read, a => write(a) :+ lit)

  /** A literal before this path: the value readers move one segment along. */
  def prefixed(lit: String): PathCodec[A] =
    PathLits.register(lit)
    PathCodec(Seg.Lit(lit) +: segments, (path, at) => read(path, at + 1), a => lit +: write(a))

  def /[B](that: PathCodec[B])(using c: Combiner[A, B]): PathCodec[c.Out] =
    PathCodec(
      segments ++ that.segments,
      PathCodec.Reader.andThen(read, width, that.read, c),
      out =>
        val (a, b) = c.separate(out)
        write(a) ++ that.write(b),
    )

  /** The same codec, matched by `unrolled` instead of the composed reader. */
  private[heddle] def withMatcher(unrolled: Chunk[String] => Option[A]): PathCodec[A] =
    PathCodec(segments, read, write, Some(unrolled))

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
  /** Reads a path's captures at a fixed offset. `matches` has already checked the length and every literal, and each
    * reader still checks its own bounds, so a reader never throws.
    */
  type Reader[A] = (Chunk[String], Int) => Option[A]

  object Reader:
    private[PathCodec] val unit: Some[Unit] = Some(())

    /** `left`, then `right` at `left`'s width; after `trailing` nothing more can match. */
    def andThen[A, B](left: Reader[A], leftWidth: Option[Int], right: Reader[B], c: Combiner[A, B]): Reader[c.Out] =
      leftWidth match
        case None    => (_, _) => None
        case Some(w) =>
          (segments, at) =>
            left(segments, at) match
              case Some(a) =>
                right(segments, at + w) match
                  case Some(b) => Some(c.combine(a, b))
                  case None    => None
              case None => None
  end Reader

  val empty: PathCodec[Unit] =
    PathCodec(Chunk.empty, (_, _) => Reader.unit, _ => Chunk.empty)

  /** Remaining path segments, including none (`/`). */
  val trailing: PathCodec[Path] =
    PathCodec(
      Chunk(Seg.Rest),
      (segments, at) => Some(Path(segments.drop(at))),
      _.segments,
    )

  inline def specialize[A](inline path: PathCodec[A]): PathCodec[A] =
    ${ PathMacros.specializeImpl[A]('path) }

  def lit(value: String): PathCodec[Unit] =
    PathLits.register(value)
    literal(value)

  private def literal(value: String): PathCodec[Unit] =
    PathCodec(
      Chunk(Seg.Lit(value)),
      (_, _) => Reader.unit,
      _ => Chunk(value),
    )

  def int(name: String): PathCodec[Int]       = variable(name, PathKind.Int32, _.toIntOption, _.toString)
  def long(name: String): PathCodec[Long]     = variable(name, PathKind.Int64, _.toLongOption, _.toString)
  def string(name: String): PathCodec[String] = variable(name, PathKind.Str, Some(_), identity)
  def uuid(name: String): PathCodec[UUID]     = variable(name, PathKind.Uuid, parseUuid, _.toString)

  private def variable[A](name: String, kind: PathKind, parse: String => Option[A], render: A => String) =
    PathCodec[A](
      Chunk(Seg.Var(name, kind)),
      (segments, at) => if at < segments.length then parse(segments(at)) else None,
      a => Chunk(render(a)),
    )

  /** RFC 9562 §4: exactly `8-4-4-4-12` hex digits. `UUID.fromString` also takes `1-2-3-4-5` and throws otherwise. */
  private[heddle] def parseUuid(raw: String): Option[UUID] =
    val groups = raw.split("-", -1)
    val shaped = groups.length == 5 && groups.map(_.length).sameElements(Array(8, 4, 4, 4, 12)) &&
      groups.forall(_.forall(c => Character.digit(c, 16) >= 0 && c < 128))
    Option.when(shaped) {
      val hex = groups.mkString
      UUID(java.lang.Long.parseUnsignedLong(hex.take(16), 16), java.lang.Long.parseUnsignedLong(hex.drop(16), 16))
    }
end PathCodec

inline def int(inline name: String): PathCodec[Int]       = PathCodec.int(name)
inline def long(inline name: String): PathCodec[Long]     = PathCodec.long(name)
inline def string(inline name: String): PathCodec[String] = PathCodec.string(name)
inline def uuid(inline name: String): PathCodec[UUID]     = PathCodec.uuid(name)
val trailing: PathCodec[Path]                             = PathCodec.trailing
