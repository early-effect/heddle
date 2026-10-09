package heddle.route

import java.util.UUID
import scala.quoted.*
import zio.Chunk

/** Unrolls a statically known `PathCodec` into a straight-line matcher. Falls back to the original codec when the path
  * is not a compile-time literal / param tree.
  */
private[heddle] object PathMacros:
  enum Part:
    case Lit(value: String)
    case I32(name: String)
    case I64(name: String)
    case Str(name: String)
    case Uuid(name: String)

  def specializeImpl[N <: Tuple: Type, A: Type](
      path: Expr[PathCodec[A] { type Names = N }]
  )(using Quotes): Expr[PathCodec[A] { type Names = N }] =
    import quotes.reflect.*
    partsOf(path.asTerm).filter(fitsFlat[A]) match
      case Some(parts) =>
        '{
          ${ registerLits(parts) }
          $path.withMatcher(${ extractFn[A](parts) })
        }
      case None => path
  end specializeImpl

  /** The unrolled matcher builds `()`, the one value, or a flat tuple; a path that groups its codecs keeps its reader.
    */
  private def fitsFlat[A: Type](parts: List[Part])(using Quotes): Boolean =
    import quotes.reflect.*
    val values = parts.collect {
      case Part.I32(_)  => TypeRepr.of[Int]
      case Part.I64(_)  => TypeRepr.of[Long]
      case Part.Str(_)  => TypeRepr.of[String]
      case Part.Uuid(_) => TypeRepr.of[UUID]
    }
    val shape = values match
      case Nil        => TypeRepr.of[Unit]
      case one :: Nil => one
      case many       => defn.TupleClass(many.length).typeRef.appliedTo(many)
    shape =:= TypeRepr.of[A]
  end fitsFlat

  private def registerLits(parts: List[Part])(using Quotes): Expr[Unit] =
    val regs = parts.collect { case Part.Lit(v) => '{ PathLits.register(${ Expr(v) }) } }
    regs match
      case Nil          => '{ () }
      case first :: all => all.foldLeft(first)((a, b) => '{ $a; $b })

  private def extractFn[A: Type](parts: List[Part])(using Quotes): Expr[Chunk[String] => Option[A]] =
    val n = Expr(parts.length)
    '{ (path: Chunk[String]) =>
      if path.length != $n then None else ${ unrolled[A]('path, parts) }
    }

  private def unrolled[A: Type](path: Expr[Chunk[String]], parts: List[Part])(using Quotes): Expr[Option[A]] =
    def rec(i: Int, vars: List[Expr[Any]]): Expr[Option[A]] =
      if i >= parts.length then finish[A](vars)
      else
        val idx = Expr(i)
        parts(i) match
          case Part.Lit(v) =>
            val lit = Expr(v)
            '{ if $path($idx) != $lit then None else ${ rec(i + 1, vars) } }
          case Part.I32(_) =>
            '{
              $path($idx).toIntOption match
                case None    => None
                case Some(x) => ${ rec(i + 1, vars :+ '{ x }) }
            }
          case Part.I64(_) =>
            '{
              $path($idx).toLongOption match
                case None    => None
                case Some(x) => ${ rec(i + 1, vars :+ '{ x }) }
            }
          case Part.Str(_) =>
            '{
              val x = $path($idx)
              ${ rec(i + 1, vars :+ '{ x }) }
            }
          case Part.Uuid(_) =>
            '{
              PathCodec.parseUuid($path($idx)) match
                case None    => None
                case Some(x) => ${ rec(i + 1, vars :+ '{ x }) }
            }
        end match
    rec(0, Nil)
  end unrolled

  /** `fitsFlat` checked the shape before expanding, and `asExprOf` has the compiler check it again. */
  private def finish[A: Type](vars: List[Expr[Any]])(using Quotes): Expr[Option[A]] =
    val value = vars match
      case Nil        => '{ () }
      case one :: Nil => one
      case many       => Expr.ofTupleFromSeq(many)
    '{ Some(${ value.asExprOf[A] }) }

  private def strip(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    def castQual(t: Term): Option[Term] = t match
      case Select(qual, name) if name.contains("asInstanceOf") => Some(qual)
      case TypeApply(fun, _)                                   => castQual(fun)
      case Apply(fun, _)                                       => castQual(fun)
      case Inlined(_, _, inner)                                => castQual(inner)
      case Typed(inner, _)                                     => castQual(inner)
      case _                                                   => None
    term match
      case Inlined(_, bindings, inner) if bindings.isEmpty => strip(inner)
      case Typed(inner, _)                                 => strip(inner)
      case other                                           =>
        castQual(other) match
          case Some(qual) => strip(qual)
          case None       =>
            val u = other.underlyingArgument
            if u == other then other else strip(u)
  end strip

  private def partsOf(using Quotes)(term: quotes.reflect.Term): Option[List[Part]] =
    import quotes.reflect.*

    def baseName(raw: String): String =
      val dropped = if raw.startsWith("inline$") then raw.drop(7) else raw
      val cut     = dropped.indexOf('$')
      if cut < 0 then dropped else dropped.substring(0, cut)

    def methodName(t: Term): String = strip(t) match
      case Apply(fun, _)     => methodName(fun)
      case TypeApply(fun, _) => methodName(fun)
      case Select(_, name)   => baseName(name)
      case Ident(name)       => baseName(name)
      case _                 => ""

    def kindName(kind: Term): String = strip(kind) match
      case Select(_, name) => name
      case Ident(name)     => name
      case _               => ""

    def isCodec(arg: Term): Boolean =
      arg.tpe.baseClasses.exists(_.fullName == "heddle.route.PathCodec")

    def valueArgs(t: Term): List[Term] = strip(t) match
      case Apply(fun, args) => valueArgs(fun) ++ args
      case _                => Nil

    def capture(args: List[Term]): Option[List[Part]] =
      val name = args.collectFirst { case Literal(StringConstant(s)) => s }
      val kind = args.map(kindName).find(Set("Int32", "Int64", "Str", "Uuid").contains)
      (name, kind) match
        case (Some(s), Some("Int32")) => Some(List(Part.I32(s)))
        case (Some(s), Some("Int64")) => Some(List(Part.I64(s)))
        case (Some(s), Some("Str"))   => Some(List(Part.Str(s)))
        case (Some(s), Some("Uuid"))  => Some(List(Part.Uuid(s)))
        case _                        => None

    def resolve(t: Term, env: Map[String, Term]): Term =
      strip(t) match
        case Ident(name) =>
          env.get(name) match
            case Some(rhs) => resolve(rhs, env - name)
            case None      => t
        case other => other

    def loop(t: Term, env: Map[String, Term]): Option[List[Part]] =
      t match
        case Inlined(_, bindings, inner) =>
          val bound = bindings.collect { case ValDef(name, _, Some(rhs)) => name -> rhs }.toMap
          loop(inner, env ++ bound)
        case _ =>
          loopStripped(t, env)

    def loopStripped(t: Term, env: Map[String, Term]): Option[List[Part]] =
      strip(t) match
        case Block(stats, expr) =>
          val more = stats.collect { case ValDef(name, _, Some(rhs)) => name -> rhs }.toMap
          loop(expr, env ++ more)
        case apply @ Apply(_, _) =>
          val args   = valueArgs(apply).map(a => resolve(a, env))
          val codecs = args.filter(isCodec)
          val lit    = args.collect { case Literal(StringConstant(s)) => s }
          methodName(apply) match
            case "capture" | "int" | "long" | "string" | "uuid" =>
              capture(args)
            case "lit" =>
              lit.headOption.map(s => List(Part.Lit(s)))
            case "prefixed" =>
              (lit, codecs) match
                case (List(s), List(codec)) => loop(codec, env).map(Part.Lit(s) :: _)
                case _                      => None
            case "appendLit" | "/" | "joined" =>
              (lit, codecs) match
                case (List(s), List(codec))   => loop(codec, env).map(_ :+ Part.Lit(s))
                case (Nil, List(left, right)) =>
                  for
                    a <- loop(left, env)
                    b <- loop(right, env)
                  yield a ++ b
                case (List(s), Nil) => Some(List(Part.Lit(s)))
                case _              => None
            case _ => None
          end match
        case Select(_, "empty") | Ident("empty") => Some(Nil)
        case _                                   => None

    loop(term, Map.empty)
  end partsOf

  def intImpl(name: Expr[String])(using Quotes): Expr[PathCodec[Int]] =
    val s = text(name)
    import quotes.reflect.*
    ConstantType(StringConstant(s)).asType match
      case '[n] =>
        '{ PathCodec.capture[n *: EmptyTuple, Int](${ Expr(s) }, PathKind.Int32, _.toIntOption, _.toString) }

  def longImpl(name: Expr[String])(using Quotes): Expr[PathCodec[Long]] =
    val s = text(name)
    import quotes.reflect.*
    ConstantType(StringConstant(s)).asType match
      case '[n] =>
        '{ PathCodec.capture[n *: EmptyTuple, Long](${ Expr(s) }, PathKind.Int64, _.toLongOption, _.toString) }

  def stringImpl(name: Expr[String])(using Quotes): Expr[PathCodec[String]] =
    val s = text(name)
    import quotes.reflect.*
    ConstantType(StringConstant(s)).asType match
      case '[n] =>
        '{ PathCodec.capture[n *: EmptyTuple, String](${ Expr(s) }, PathKind.Str, Some(_), identity) }

  def uuidImpl(name: Expr[String])(using Quotes): Expr[PathCodec[UUID]] =
    val s = text(name)
    import quotes.reflect.*
    ConstantType(StringConstant(s)).asType match
      case '[n] =>
        '{ PathCodec.capture[n *: EmptyTuple, UUID](${ Expr(s) }, PathKind.Uuid, PathCodec.parseUuid, _.toString) }

  private def text(name: Expr[String])(using Quotes): String =
    import quotes.reflect.*
    name.asTerm match
      case Inlined(_, _, Literal(StringConstant(s))) => s
      case Literal(StringConstant(s))                => s
      case other => report.errorAndAbort(s"a capture name must be a literal, got ${other.show}")
end PathMacros
