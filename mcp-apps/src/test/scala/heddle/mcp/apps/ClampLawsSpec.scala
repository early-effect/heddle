package heddle.mcp.apps

import heddle.mcp.apps.AppGens.*
import zio.*
import zio.test.*

object ClampLawsSpec extends ZIOSpecDefault:
  private val c = Clamp[UiPolicy, HostPolicy]

  /** `a` grants no more than `b`: every origin, permission, and stable origin in `a` is in `b`. */
  private def within(a: UiPolicy, b: UiPolicy): Boolean =
    a.network.connect.subsetOf(b.network.connect) && a.network.resources.subsetOf(b.network.resources) &&
      a.network.frames.subsetOf(b.network.frames) && a.network.base.subsetOf(b.network.base) &&
      a.permissions.subsetOf(b.permissions) && a.border == b.border && ((a.origin, b.origin) match
        case (AppOrigin.Opaque, _)                      => true
        case (AppOrigin.Stable(x), AppOrigin.Stable(y)) => x == y
        case _                                          => false)

  /** For every asked origin under an allowance that names origins, whether that allowance keeps it. */
  private def verdicts(ask: Network, allow: NetworkAllowance): List[Boolean] =
    List(
      ask.connect   -> allow.connect,
      ask.resources -> allow.resources,
      ask.frames    -> allow.frames,
      ask.base      -> allow.base,
    ).flatMap:
      case (asked, Admit.Only(os)) => asked.toList.map(os.contains)
      case (_, Admit.AnyOrigin)    => Nil

  def spec = suite("Clamp laws")(
    test("the laws are not vacuous: an allowance that names origins keeps some asked origins and drops others"):
      (policy <*> host).runCollectN(200).map { pairs =>
        val all     = pairs.flatMap((ask, allow) => verdicts(ask.network, allow.network))
        val kept    = all.count(identity)
        val dropped = all.size - kept
        assertTrue(kept > 0, dropped > 0)
      }
    ,
    test("it only narrows: the result never grants more than the ask"):
      check(policy, host)((ask, allow) => assertTrue(within(c.clamp(ask, allow), ask)))
    ,
    test("it is idempotent"):
      check(policy, host) { (ask, allow) =>
        val once = c.clamp(ask, allow)
        assertTrue(c.clamp(once, allow) == once)
      }
    ,
    test("a host that allows everything changes nothing"):
      check(policy)(ask => assertTrue(c.clamp(ask, HostPolicy.open) == ask, c.narrowed(ask, HostPolicy.open).isEmpty))
    ,
    test("a host that allows nothing leaves an isolated, opaque view with no permissions"):
      check(policy) { ask =>
        val out = c.clamp(ask, HostPolicy.closed)
        assertTrue(
          out.network.isolated,
          out.permissions.isEmpty,
          out.origin == AppOrigin.Opaque,
          out.border == ask.border,
        )
      }
    ,
    test("it is monotone: a wider allowance never yields a narrower result"):
      check(policy, widening) { case (ask, (narrow, wide)) =>
        assertTrue(within(c.clamp(ask, narrow), c.clamp(ask, wide)))
      }
    ,
    test("narrowed is empty exactly when the clamp returns the ask"):
      check(policy, host)((ask, allow) => assertTrue(c.narrowed(ask, allow).isEmpty == (c.clamp(ask, allow) == ask)))
    ,
    test("narrowed names exactly what was taken away"):
      check(policy, host) { (ask, allow) =>
        val out     = c.clamp(ask, allow)
        val dropped = c.narrowed(ask, allow).collect { case Narrowing.OriginDropped(d, o) => d -> o }.toSet
        val lost    = Set(
          Directive.Connect  -> (ask.network.connect -- out.network.connect),
          Directive.Resource -> (ask.network.resources -- out.network.resources),
          Directive.Frame    -> (ask.network.frames -- out.network.frames),
          Directive.Base     -> (ask.network.base -- out.network.base),
        ).flatMap((d, os) => os.map(d -> _))
        assertTrue(dropped == lost)
      },
  ) @@ TestAspect.timeout(60.seconds)
end ClampLawsSpec
