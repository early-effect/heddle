package heddle.mcp.apps

/** Who may call a tool: the model, the view, or both (the MCP Apps default). */
enum Visibility:
  case Model, App, ModelAndApp

  def model: Boolean = this != App
  def app: Boolean   = this != Model

/** A browser capability a view may ask for (Permission Policy features). */
/** `wire` is the `_meta.ui.permissions` key; `feature` is the Permissions Policy name an iframe's `allow` delegates. */
enum Permission(val wire: String, val feature: String):
  case Camera         extends Permission("camera", "camera")
  case Microphone     extends Permission("microphone", "microphone")
  case Geolocation    extends Permission("geolocation", "geolocation")
  case ClipboardWrite extends Permission("clipboardWrite", "clipboard-write")

object Permission:
  /** An iframe `allow` value delegating `ps`, or `None` for none. */
  def allow(ps: Set[Permission]): Option[String] =
    Option.when(ps.nonEmpty)(ps.toList.sortBy(_.ordinal).map(_.feature).mkString("; "))

/** Where the view's document runs. `Opaque` is a fresh origin per mount; `Stable` asks the host to mint a lasting one,
  * for APIs that allowlist an `Origin`. The label names it; its format is the host's.
  */
enum AppOrigin:
  case Opaque
  case Stable(label: String)

/** Whether the view wants the host to draw a border and background around it. */
enum Border:
  case Visible, Hidden, HostDefault

/** The origins a view reaches, by CSP directive. All empty is an isolated view: no network at all. */
final case class Network(
    connect: Set[Origin] = Set.empty,
    resources: Set[Origin] = Set.empty,
    frames: Set[Origin] = Set.empty,
    base: Set[Origin] = Set.empty,
):
  def isolated: Boolean = connect.isEmpty && resources.isEmpty && frames.isEmpty && base.isEmpty

object Network:
  val isolated: Network = Network()

/** What a view asks of its host: the resource's `_meta.ui`. Closed by default. */
final case class UiPolicy(
    network: Network = Network.isolated,
    permissions: Set[Permission] = Set.empty,
    origin: AppOrigin = AppOrigin.Opaque,
    border: Border = Border.HostDefault,
)

object UiPolicy:
  val closed: UiPolicy = UiPolicy()

/** Which origins a host lets a view reach for one directive. */
enum Admit:
  case AnyOrigin
  case Only(origins: Set[Origin])

  def allows(o: Origin): Boolean =
    this match
      case AnyOrigin => true
      case Only(os)  => os.contains(o)

object Admit:
  val none: Admit = Only(Set.empty)

/** The network a host allows, by directive. */
final case class NetworkAllowance(connect: Admit, resources: Admit, frames: Admit, base: Admit)

object NetworkAllowance:
  val any: NetworkAllowance  = NetworkAllowance(Admit.AnyOrigin, Admit.AnyOrigin, Admit.AnyOrigin, Admit.AnyOrigin)
  val none: NetworkAllowance = NetworkAllowance(Admit.none, Admit.none, Admit.none, Admit.none)

/** Whether a host mints stable origins. */
enum StableOrigins:
  case Mint, Refuse

/** What a host allows a view. `open` honors every ask; `closed` narrows every view to the closed policy. */
final case class HostPolicy(network: NetworkAllowance, permissions: Set[Permission], stable: StableOrigins)

object HostPolicy:
  val open: HostPolicy   = HostPolicy(NetworkAllowance.any, Permission.values.toSet, StableOrigins.Mint)
  val closed: HostPolicy = HostPolicy(NetworkAllowance.none, Set.empty, StableOrigins.Refuse)
