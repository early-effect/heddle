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

  def specializeImpl[A: Type](path: Expr[PathCodec[A]])(using Quotes): Expr[PathCodec[A]] =
    import quotes.reflect.*
    partsOf(path.asTerm) match
      case Some(parts) =>
        val segs = '{ Chunk.fromIterable(${ Expr.ofList(parts.map(segExpr)) }) }
        '{
          ${ registerLits(parts) }
          PathCodec($segs, ${ extractFn[A](parts) }, true)
        }
      case None => path
  end specializeImpl

  private def registerLits(parts: List[Part])(using Quotes): Expr[Unit] =
    val regs = parts.collect { case Part.Lit(v) => '{ PathLits.register(${ Expr(v) }) } }
    regs match
      case Nil      => '{ () }
      case r :: Nil => r
      case rs       => rs.reduce((a, b) => '{ $a; $b })

  private def segExpr(part: Part)(using Quotes): Expr[Seg] =
    part match
      case Part.Lit(v)  => '{ Seg.Lit(${ Expr(v) }) }
      case Part.I32(n)  => '{ Seg.Var(${ Expr(n) }, PathKind.Int32) }
      case Part.I64(n)  => '{ Seg.Var(${ Expr(n) }, PathKind.Int64) }
      case Part.Str(n)  => '{ Seg.Var(${ Expr(n) }, PathKind.Str) }
      case Part.Uuid(n) => '{ Seg.Var(${ Expr(n) }, PathKind.Uuid) }

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
              try
                val x = UUID.fromString($path($idx))
                ${ rec(i + 1, vars :+ '{ x }) }
              catch case _: IllegalArgumentException => None
            }
        end match
    rec(0, Nil)
  end unrolled

  private def finish[A: Type](vars: List[Expr[Any]])(using Quotes): Expr[Option[A]] =
    vars match
      case Nil =>
        '{ Some(()).asInstanceOf[Option[A]] }
      case v :: Nil =>
        '{ Some($v.asInstanceOf[A]) }
      case v1 :: v2 :: rest =>
        val tup: Expr[Any] =
          rest.foldLeft[Expr[Any]]('{ ($v1, $v2) })((acc, v) => '{ ($acc, $v) })
        '{ Some($tup.asInstanceOf[A]) }

  private def strip(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, _, inner) => strip(inner)
      case Typed(inner, _)      => strip(inner)
      case _                    =>
        val u = term.underlyingArgument
        if u == term then term else strip(u)

  private def partsOf(using Quotes)(term: quotes.reflect.Term): Option[List[Part]] =
    import quotes.reflect.*

    def named(n: String, s: String): Option[List[Part]] =
      n match
        case "lit"    => Some(List(Part.Lit(s)))
        case "int"    => Some(List(Part.I32(s)))
        case "long"   => Some(List(Part.I64(s)))
        case "string" => Some(List(Part.Str(s)))
        case "uuid"   => Some(List(Part.Uuid(s)))
        case _        => None

    def loop(t: Term): Option[List[Part]] =
      strip(t) match
        case Apply(fun, args) =>
          val core = fun match
            case TypeApply(c, _) => c
            case c               => c
          val name = core.symbol.name
          (name, core, args) match
            case ("/", Select(qual, "/"), List(Literal(StringConstant(s)))) =>
              loop(qual).map(_ :+ Part.Lit(s))
            case ("/", Select(qual, "/"), List(arg)) =>
              for
                a <- loop(qual)
                b <- loop(arg)
              yield a ++ b
            case ("/", Apply(inner, List(Literal(StringConstant(s)))), List(arg)) if inner.symbol.name == "/" =>
              loop(arg).map(Part.Lit(s) :: _)
            case (n, _, List(Literal(StringConstant(s)))) =>
              named(n, s)
            case _ =>
              core match
                case Apply(inner, List(Literal(StringConstant(s)))) if inner.symbol.name == "/" =>
                  args match
                    case List(arg) => loop(arg).map(Part.Lit(s) :: _)
                    case _         => None
                case _ => None
          end match
        case Select(_, "empty") => Some(Nil)
        case Ident("empty")     => Some(Nil)
        case _                  => None

    loop(term)
  end partsOf
end PathMacros
