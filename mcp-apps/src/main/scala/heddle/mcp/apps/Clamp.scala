package heddle.mcp.apps

import zio.Chunk

/** One thing a host took away from what a view asked. The audit log records each. */
enum Narrowing:
  case OriginDropped(directive: Directive, origin: Origin)
  case PermissionDropped(permission: Permission)
  case StableOriginRefused(label: String)

/** One of a view's network lists, named by its `_meta.ui.csp` key. */
enum Directive(val wire: String):
  case Connect  extends Directive("connectDomains")
  case Resource extends Directive("resourceDomains")
  case Frame    extends Directive("frameDomains")
  case Base     extends Directive("baseUriDomains")

/** Clamps what a view asks by what a host allows. The laws, checked in `ClampLawsSpec`:
  *
  *   - it only narrows: the result never grants more than the ask
  *   - it is idempotent: clamping twice by the same allowance is clamping once
  *   - a host that allows everything changes nothing; one that allows nothing leaves nothing
  *   - it is monotone: a wider allowance never yields a narrower result
  *   - `narrowed` is empty exactly when the clamp returns the ask
  */
trait Clamp[Ask, Allowance]:
  def clamp(ask: Ask, allow: Allowance): Ask
  def narrowed(ask: Ask, allow: Allowance): Chunk[Narrowing]

object Clamp:
  def apply[A, H](using c: Clamp[A, H]): Clamp[A, H] = c

  given network: Clamp[Network, NetworkAllowance] with
    def clamp(ask: Network, allow: NetworkAllowance): Network =
      Network(
        ask.connect.filter(allow.connect.allows),
        ask.resources.filter(allow.resources.allows),
        ask.frames.filter(allow.frames.allows),
        ask.base.filter(allow.base.allows),
      )

    def narrowed(ask: Network, allow: NetworkAllowance): Chunk[Narrowing] =
      def dropped(d: Directive, asked: Set[Origin], admit: Admit) =
        Chunk.fromIterable(asked.filterNot(admit.allows).toList.sortBy(_.render)).map(Narrowing.OriginDropped(d, _))
      dropped(Directive.Connect, ask.connect, allow.connect) ++
        dropped(Directive.Resource, ask.resources, allow.resources) ++
        dropped(Directive.Frame, ask.frames, allow.frames) ++
        dropped(Directive.Base, ask.base, allow.base)
  end network

  given permissions: Clamp[Set[Permission], Set[Permission]] with
    def clamp(ask: Set[Permission], allow: Set[Permission]): Set[Permission]     = ask.intersect(allow)
    def narrowed(ask: Set[Permission], allow: Set[Permission]): Chunk[Narrowing] =
      Chunk.fromIterable(ask.diff(allow).toList.sortBy(_.ordinal)).map(Narrowing.PermissionDropped(_))

  given origin: Clamp[AppOrigin, StableOrigins] with
    def clamp(ask: AppOrigin, allow: StableOrigins): AppOrigin =
      (ask, allow) match
        case (AppOrigin.Stable(_), StableOrigins.Refuse) => AppOrigin.Opaque
        case (other, _)                                  => other

    def narrowed(ask: AppOrigin, allow: StableOrigins): Chunk[Narrowing] =
      (ask, allow) match
        case (AppOrigin.Stable(label), StableOrigins.Refuse) => Chunk(Narrowing.StableOriginRefused(label))
        case _                                               => Chunk.empty
  end origin

  /** Field by field. The border is the view's preference and the host's to draw, so it is never clamped. */
  given policy: Clamp[UiPolicy, HostPolicy] with
    def clamp(ask: UiPolicy, allow: HostPolicy): UiPolicy =
      ask.copy(
        network = network.clamp(ask.network, allow.network),
        permissions = permissions.clamp(ask.permissions, allow.permissions),
        origin = origin.clamp(ask.origin, allow.stable),
      )

    def narrowed(ask: UiPolicy, allow: HostPolicy): Chunk[Narrowing] =
      network.narrowed(ask.network, allow.network) ++
        permissions.narrowed(ask.permissions, allow.permissions) ++
        origin.narrowed(ask.origin, allow.stable)
  end policy
end Clamp
