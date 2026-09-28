package heddle.mcp.apps.ui

import heddle.mcp.apps.{MetaProblem, Network, Permission, UiMeta}
import heddle.mcp.protocol.{Implementation, RequestId, Tool}
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** SEP-1865 MCP Apps, the view protocol: what a view and its host say over `postMessage`. */
object UiProtocol:
  /** `LATEST_PROTOCOL_VERSION` in the ext-apps SDK. */
  val Version = "2026-01-26"

  /** A codec for an enum whose cases travel as fixed strings. */
  private[ui] def wire[A](values: Array[A], name: A => String, what: String): JsonCodec[A] =
    JsonCodec(
      JsonEncoder.string.contramap(name),
      JsonDecoder.string.mapOrFail(s => values.find(name(_) == s).toRight(s"not a $what: $s")),
    )
end UiProtocol

enum DisplayMode(val wire: String):
  case Inline     extends DisplayMode("inline")
  case Fullscreen extends DisplayMode("fullscreen")
  case Pip        extends DisplayMode("pip")

object DisplayMode:
  given JsonCodec[DisplayMode] = UiProtocol.wire(values, _.wire, "display mode")

enum Theme(val wire: String):
  case Light extends Theme("light")
  case Dark  extends Theme("dark")

object Theme:
  given JsonCodec[Theme] = UiProtocol.wire(values, _.wire, "theme")

enum Platform(val wire: String):
  case Web     extends Platform("web")
  case Desktop extends Platform("desktop")
  case Mobile  extends Platform("mobile")

object Platform:
  given JsonCodec[Platform] = UiProtocol.wire(values, _.wire, "platform")

/** The standardized theme variables (`McpUiStyleVariableKey`); a host may send any subset. */
enum HostVar(val wire: String):
  case ColorBackgroundPrimary   extends HostVar("--color-background-primary")
  case ColorBackgroundSecondary extends HostVar("--color-background-secondary")
  case ColorBackgroundTertiary  extends HostVar("--color-background-tertiary")
  case ColorBackgroundInverse   extends HostVar("--color-background-inverse")
  case ColorBackgroundGhost     extends HostVar("--color-background-ghost")
  case ColorBackgroundInfo      extends HostVar("--color-background-info")
  case ColorBackgroundDanger    extends HostVar("--color-background-danger")
  case ColorBackgroundSuccess   extends HostVar("--color-background-success")
  case ColorBackgroundWarning   extends HostVar("--color-background-warning")
  case ColorBackgroundDisabled  extends HostVar("--color-background-disabled")
  case ColorTextPrimary         extends HostVar("--color-text-primary")
  case ColorTextSecondary       extends HostVar("--color-text-secondary")
  case ColorTextTertiary        extends HostVar("--color-text-tertiary")
  case ColorTextInverse         extends HostVar("--color-text-inverse")
  case ColorTextGhost           extends HostVar("--color-text-ghost")
  case ColorTextInfo            extends HostVar("--color-text-info")
  case ColorTextDanger          extends HostVar("--color-text-danger")
  case ColorTextSuccess         extends HostVar("--color-text-success")
  case ColorTextWarning         extends HostVar("--color-text-warning")
  case ColorTextDisabled        extends HostVar("--color-text-disabled")
  case ColorBorderPrimary       extends HostVar("--color-border-primary")
  case ColorBorderSecondary     extends HostVar("--color-border-secondary")
  case ColorBorderTertiary      extends HostVar("--color-border-tertiary")
  case ColorBorderInverse       extends HostVar("--color-border-inverse")
  case ColorBorderGhost         extends HostVar("--color-border-ghost")
  case ColorBorderInfo          extends HostVar("--color-border-info")
  case ColorBorderDanger        extends HostVar("--color-border-danger")
  case ColorBorderSuccess       extends HostVar("--color-border-success")
  case ColorBorderWarning       extends HostVar("--color-border-warning")
  case ColorBorderDisabled      extends HostVar("--color-border-disabled")
  case ColorRingPrimary         extends HostVar("--color-ring-primary")
  case ColorRingSecondary       extends HostVar("--color-ring-secondary")
  case ColorRingInverse         extends HostVar("--color-ring-inverse")
  case ColorRingInfo            extends HostVar("--color-ring-info")
  case ColorRingDanger          extends HostVar("--color-ring-danger")
  case ColorRingSuccess         extends HostVar("--color-ring-success")
  case ColorRingWarning         extends HostVar("--color-ring-warning")
  case FontSans                 extends HostVar("--font-sans")
  case FontMono                 extends HostVar("--font-mono")
  case FontWeightNormal         extends HostVar("--font-weight-normal")
  case FontWeightMedium         extends HostVar("--font-weight-medium")
  case FontWeightSemibold       extends HostVar("--font-weight-semibold")
  case FontWeightBold           extends HostVar("--font-weight-bold")
  case FontTextXsSize           extends HostVar("--font-text-xs-size")
  case FontTextSmSize           extends HostVar("--font-text-sm-size")
  case FontTextMdSize           extends HostVar("--font-text-md-size")
  case FontTextLgSize           extends HostVar("--font-text-lg-size")
  case FontHeadingXsSize        extends HostVar("--font-heading-xs-size")
  case FontHeadingSmSize        extends HostVar("--font-heading-sm-size")
  case FontHeadingMdSize        extends HostVar("--font-heading-md-size")
  case FontHeadingLgSize        extends HostVar("--font-heading-lg-size")
  case FontHeadingXlSize        extends HostVar("--font-heading-xl-size")
  case FontHeading2xlSize       extends HostVar("--font-heading-2xl-size")
  case FontHeading3xlSize       extends HostVar("--font-heading-3xl-size")
  case FontTextXsLineHeight     extends HostVar("--font-text-xs-line-height")
  case FontTextSmLineHeight     extends HostVar("--font-text-sm-line-height")
  case FontTextMdLineHeight     extends HostVar("--font-text-md-line-height")
  case FontTextLgLineHeight     extends HostVar("--font-text-lg-line-height")
  case FontHeadingXsLineHeight  extends HostVar("--font-heading-xs-line-height")
  case FontHeadingSmLineHeight  extends HostVar("--font-heading-sm-line-height")
  case FontHeadingMdLineHeight  extends HostVar("--font-heading-md-line-height")
  case FontHeadingLgLineHeight  extends HostVar("--font-heading-lg-line-height")
  case FontHeadingXlLineHeight  extends HostVar("--font-heading-xl-line-height")
  case FontHeading2xlLineHeight extends HostVar("--font-heading-2xl-line-height")
  case FontHeading3xlLineHeight extends HostVar("--font-heading-3xl-line-height")
  case BorderRadiusXs           extends HostVar("--border-radius-xs")
  case BorderRadiusSm           extends HostVar("--border-radius-sm")
  case BorderRadiusMd           extends HostVar("--border-radius-md")
  case BorderRadiusLg           extends HostVar("--border-radius-lg")
  case BorderRadiusXl           extends HostVar("--border-radius-xl")
  case BorderRadiusFull         extends HostVar("--border-radius-full")
  case BorderWidthRegular       extends HostVar("--border-width-regular")
  case ShadowHairline           extends HostVar("--shadow-hairline")
  case ShadowSm                 extends HostVar("--shadow-sm")
  case ShadowMd                 extends HostVar("--shadow-md")
  case ShadowLg                 extends HostVar("--shadow-lg")
end HostVar

object HostVar:
  def fromWire(s: String): Option[HostVar] = values.find(_.wire == s)

/** What a host accepts in a message or a model-context update. */
enum Modality(val wire: String):
  case Text              extends Modality("text")
  case Image             extends Modality("image")
  case Audio             extends Modality("audio")
  case Resource          extends Modality("resource")
  case ResourceLink      extends Modality("resourceLink")
  case StructuredContent extends Modality("structuredContent")

object Modality:
  /** One empty object per modality, as the wire writes a set. */
  given JsonCodec[Set[Modality]] =
    JsonCodec(
      Json.encoder.contramap(ms => Json.Obj(Chunk.fromIterable(ms.toList.sortBy(_.ordinal)).map(_.wire -> Json.Obj()))),
      Json.decoder.mapOrFail {
        case o: Json.Obj => Right(o.fields.flatMap((k, _) => values.find(_.wire == k)).toSet)
        case other       => Left(s"not a set of modalities: $other")
      },
    )
end Modality

/** A capability that is on when present: the wire's `{}`. */
case object Present:
  given JsonCodec[Present.type] =
    JsonCodec(
      Json.encoder.contramap(_ => Json.Obj()),
      Json.decoder.mapOrFail {
        case _: Json.Obj => Right(Present)
        case other       => Left(s"expected {}, got $other")
      },
    )

/** One side of the view's container: fixed by the host, bounded, or the view's to choose. */
enum Extent:
  case Fixed(px: Double)
  case AtMost(px: Double)
  case Free

/** `containerDimensions`: height and width, each fixed (`height`), bounded (`maxHeight`), or free (absent). */
final case class ContainerDimensions(height: Extent, width: Extent)

object ContainerDimensions:
  given JsonCodec[ContainerDimensions] =
    def side(fixed: String, max: String)(e: Extent): Chunk[(String, Json)] =
      e match
        case Extent.Fixed(px)  => Chunk(fixed -> Json.Num(px))
        case Extent.AtMost(px) => Chunk(max -> Json.Num(px))
        case Extent.Free       => Chunk.empty
    def read(o: Json.Obj, fixed: String, max: String): Extent =
      (o.get(fixed), o.get(max)) match
        case (Some(Json.Num(n)), _) => Extent.Fixed(n.doubleValue)
        case (_, Some(Json.Num(n))) => Extent.AtMost(n.doubleValue)
        case _                      => Extent.Free
    JsonCodec(
      Json.encoder.contramap(d =>
        Json.Obj(side("height", "maxHeight")(d.height) ++ side("width", "maxWidth")(d.width))
      ),
      Json.decoder.mapOrFail {
        case o: Json.Obj => Right(ContainerDimensions(read(o, "height", "maxHeight"), read(o, "width", "maxWidth")))
        case other       => Left(s"not container dimensions: $other")
      },
    )
  end given
end ContainerDimensions

final case class DeviceCapabilities(touch: Option[Boolean] = None, hover: Option[Boolean] = None) derives JsonCodec

final case class SafeAreaInsets(top: Double, right: Double, bottom: Double, left: Double) derives JsonCodec

/** The tool call that opened this view: its JSON-RPC id and definition. */
final case class ToolInfo(id: Option[RequestId], tool: Tool) derives JsonCodec

/** Theme variables the host sets, and font CSS (`@font-face` or `@import`) a view may inject. */
final case class HostStyles(variables: Map[HostVar, String] = Map.empty, fonts: Option[String] = None)

object HostStyles:
  given JsonCodec[HostStyles] =
    JsonCodec(
      Json.encoder.contramap { s =>
        val vars = Chunk.fromIterable(s.variables.toList.sortBy(_._1.ordinal)).map((k, v) => k.wire -> Json.Str(v))
        Json.Obj(
          Chunk.fromIterable(Option.when(vars.nonEmpty)("variables" -> Json.Obj(vars))) ++
            Chunk.fromIterable(s.fonts.map(f => "css" -> Json.Obj("fonts" -> Json.Str(f))))
        )
      },
      Json.decoder.mapOrFail {
        case o: Json.Obj =>
          val vars = o.get("variables").collect { case v: Json.Obj => v.fields }.getOrElse(Chunk.empty).flatMap {
            case (k, Json.Str(v)) => HostVar.fromWire(k).map(_ -> v)
            case _                => None
          }
          val fonts =
            o.get("css").collect { case c: Json.Obj => c }.flatMap(_.get("fonts")).collect { case Json.Str(f) => f }
          Right(HostStyles(vars.toMap, fonts))
        case other => Left(s"not host styles: $other")
      },
    )
end HostStyles

/** `McpUiHostContext`. Every field is optional because `host-context-changed` sends only what changed; `extra` keeps
  * the fields a newer host sends that this revision does not know.
  */
final case class HostContext(
    toolInfo: Option[ToolInfo] = None,
    theme: Option[Theme] = None,
    styles: Option[HostStyles] = None,
    displayMode: Option[DisplayMode] = None,
    availableDisplayModes: Option[Chunk[DisplayMode]] = None,
    containerDimensions: Option[ContainerDimensions] = None,
    locale: Option[String] = None,
    timeZone: Option[String] = None,
    userAgent: Option[String] = None,
    platform: Option[Platform] = None,
    deviceCapabilities: Option[DeviceCapabilities] = None,
    safeAreaInsets: Option[SafeAreaInsets] = None,
    extra: Json.Obj = Json.Obj(),
):
  /** A `host-context-changed` patch: each field it names replaces this one's, and the rest stay. */
  def merge(patch: HostContext): HostContext =
    HostContext(
      patch.toolInfo.orElse(toolInfo),
      patch.theme.orElse(theme),
      patch.styles.orElse(styles),
      patch.displayMode.orElse(displayMode),
      patch.availableDisplayModes.orElse(availableDisplayModes),
      patch.containerDimensions.orElse(containerDimensions),
      patch.locale.orElse(locale),
      patch.timeZone.orElse(timeZone),
      patch.userAgent.orElse(userAgent),
      patch.platform.orElse(platform),
      patch.deviceCapabilities.orElse(deviceCapabilities),
      patch.safeAreaInsets.orElse(safeAreaInsets),
      Json.Obj(extra.fields.filterNot((k, _) => patch.extra.get(k).isDefined) ++ patch.extra.fields),
    )
end HostContext

object HostContext:
  val empty: HostContext = HostContext()

  private val known = Set(
    "toolInfo",
    "theme",
    "styles",
    "displayMode",
    "availableDisplayModes",
    "containerDimensions",
    "locale",
    "timeZone",
    "userAgent",
    "platform",
    "deviceCapabilities",
    "safeAreaInsets",
  )

  given JsonCodec[HostContext] =
    def field[A: JsonEncoder](name: String, a: Option[A]): Chunk[(String, Json)] =
      Chunk.fromIterable(a.flatMap(v => v.toJsonAST.toOption).map(name -> _))
    def read[A: JsonDecoder](o: Json.Obj, name: String): Either[String, Option[A]] =
      o.get(name) match
        case None | Some(Json.Null) => Right(None)
        case Some(j)                => j.as[A].map(Some(_)).left.map(e => s"$name: $e")
    JsonCodec(
      Json.encoder.contramap { c =>
        Json.Obj(
          field("toolInfo", c.toolInfo) ++ field("theme", c.theme) ++ field("styles", c.styles) ++
            field("displayMode", c.displayMode) ++ field("availableDisplayModes", c.availableDisplayModes) ++
            field("containerDimensions", c.containerDimensions) ++ field("locale", c.locale) ++
            field("timeZone", c.timeZone) ++ field("userAgent", c.userAgent) ++ field("platform", c.platform) ++
            field("deviceCapabilities", c.deviceCapabilities) ++ field("safeAreaInsets", c.safeAreaInsets) ++
            c.extra.fields
        )
      },
      Json.decoder.mapOrFail {
        case o: Json.Obj =>
          for
            toolInfo   <- read[ToolInfo](o, "toolInfo")
            theme      <- read[Theme](o, "theme")
            styles     <- read[HostStyles](o, "styles")
            mode       <- read[DisplayMode](o, "displayMode")
            modes      <- read[Chunk[DisplayMode]](o, "availableDisplayModes")
            dimensions <- read[ContainerDimensions](o, "containerDimensions")
            locale     <- read[String](o, "locale")
            timeZone   <- read[String](o, "timeZone")
            userAgent  <- read[String](o, "userAgent")
            platform   <- read[Platform](o, "platform")
            device     <- read[DeviceCapabilities](o, "deviceCapabilities")
            insets     <- read[SafeAreaInsets](o, "safeAreaInsets")
          yield HostContext(
            toolInfo,
            theme,
            styles,
            mode,
            modes,
            dimensions,
            locale,
            timeZone,
            userAgent,
            platform,
            device,
            insets,
            Json.Obj(o.fields.filterNot((k, _) => known(k))),
          )
        case other => Left(s"not a host context: $other")
      },
    )
  end given
end HostContext

/** `{ listChanged?: boolean }` on the tool and resource capabilities. */
final case class ListChanged(listChanged: Option[Boolean] = None) derives JsonCodec

/** The sandbox the host granted: the CSP origins and browser permissions it applied. */
final case class SandboxGrant(network: Network = Network.isolated, permissions: Set[Permission] = Set.empty)

object SandboxGrant:
  given JsonCodec[SandboxGrant] =
    JsonCodec(
      Json.encoder.contramap { g =>
        val csp  = UiMeta.encodeCsp(g.network)
        val perm = UiMeta.encodePermissions(g.permissions)
        Json.Obj(
          Chunk.fromIterable(Option.when(csp.fields.nonEmpty)("csp" -> csp)) ++
            Chunk.fromIterable(Option.when(perm.fields.nonEmpty)("permissions" -> perm))
        )
      },
      Json.decoder.mapOrFail {
        case o: Json.Obj => Right(read(o)._1)
        case other       => Left(s"not a sandbox grant: $other")
      },
    )

  /** Refuses a grant that loses anything in decoding, where the lenient codec drops it. For whoever must run under
    * exactly what was written: a relay, or the server that compiles a relay's CSP.
    */
  def strict(json: Json): Either[String, SandboxGrant] =
    json match
      case o: Json.Obj =>
        val (grant, problems) = read(o)
        Either.cond(
          problems.isEmpty,
          grant,
          s"the grant has entries that cannot be honored: ${problems.mkString(", ")}",
        )
      case other => Left(s"not a sandbox grant: $other")

  private def read(o: Json.Obj): (SandboxGrant, Chunk[MetaProblem]) =
    val obj                   = (k: String) => o.get(k).collect { case v: Json.Obj => v }.getOrElse(Json.Obj())
    val (network, badOrigins) = UiMeta.decodeCsp(obj("csp"))
    val (perms, badPerms)     = UiMeta.decodePermissions(obj("permissions"))
    (SandboxGrant(network, perms), badOrigins ++ badPerms)
end SandboxGrant

/** `McpUiHostCapabilities`: what the host offers the view. */
final case class HostCapabilities(
    openLinks: Option[Present.type] = None,
    downloadFile: Option[Present.type] = None,
    serverTools: Option[ListChanged] = None,
    serverResources: Option[ListChanged] = None,
    logging: Option[Present.type] = None,
    sandbox: Option[SandboxGrant] = None,
    updateModelContext: Option[Set[Modality]] = None,
    message: Option[Set[Modality]] = None,
    sampling: Option[Json.Obj] = None,
    experimental: Option[Json.Obj] = None,
) derives JsonCodec

/** `McpUiAppCapabilities`: what the view supports. */
final case class AppCapabilities(
    availableDisplayModes: Option[Chunk[DisplayMode]] = None,
    tools: Option[ListChanged] = None,
    experimental: Option[Json.Obj] = None,
) derives JsonCodec

/** The host's answer to `ui/initialize`. */
final case class InitializeResult(
    protocolVersion: String,
    hostInfo: Implementation,
    hostCapabilities: HostCapabilities,
    hostContext: HostContext,
) derives JsonCodec

/** How a host answered `ui/open-link`, `ui/message`, or `ui/download-file`: done, or refused (`isError: true`). */
enum Outcome:
  case Done, Refused

object Outcome:
  def fromWire(result: Json.Obj): Outcome =
    if result.get("isError").contains(Json.Bool(true)) then Refused else Done

  def wire(o: Outcome): Json.Obj =
    o match
      case Done    => Json.Obj()
      case Refused => Json.Obj("isError" -> Json.Bool(true))
