package heddle.mcp.apps

import zio.Chunk
import zio.json.ast.Json

/** A tool's `_meta.ui`: the view it renders in, and who may call it. */
final case class ToolUi(resourceUri: Option[UiUri], visibility: Visibility)

/** What a host could not accept while reading `_meta.ui`. Unreadable asks are dropped, never widened. */
enum MetaProblem:
  case BadOrigin(directive: Directive, raw: String, reason: OriginError)
  case BadResourceUri(raw: String)
  case UnknownPermission(name: String)

/** `_meta.ui` in both directions. `encode*` writes what a Heddle server sends; `decode*` reads any server's, keeps what
  * it can trust, and reports what it dropped.
  */
object UiMeta:
  val Key               = "ui"
  val LegacyResourceKey = "ui/resourceUri"

  /** `text/html;profile=mcp-app`: an MCP App view. */
  val MimeType = "text/html;profile=mcp-app"

  def encodeTool(ui: ToolUi): Json.Obj =
    val visibility = ui.visibility match
      case Visibility.Model       => Chunk("model")
      case Visibility.App         => Chunk("app")
      case Visibility.ModelAndApp => Chunk("model", "app")
    val fields = Chunk.fromIterable(ui.resourceUri.map(u => "resourceUri" -> Json.Str(u.value))) :+
      ("visibility" -> Json.Arr(visibility.map(Json.Str(_))))
    Json.Obj(Key    -> Json.Obj(fields))

  /** A missing or empty visibility is the spec default, both. */
  def decodeTool(meta: Option[Json.Obj]): (ToolUi, Chunk[MetaProblem]) =
    val ui     = meta.flatMap(_.get(Key)).collect { case o: Json.Obj => o }
    val rawUri = ui.flatMap(_.get("resourceUri")).orElse(meta.flatMap(_.get(LegacyResourceKey))).collect {
      case Json.Str(s) => s
    }
    val uri = rawUri.map(s => UiUri.from(s).left.map(_ => MetaProblem.BadResourceUri(s)))
    val who = ui.flatMap(_.get("visibility")).collect { case Json.Arr(vs) => vs.collect { case Json.Str(s) => s } }
    val vis = who.map(_.toSet).filter(_.nonEmpty) match
      case Some(s) if s == Set("model") => Visibility.Model
      case Some(s) if s == Set("app")   => Visibility.App
      case _                            => Visibility.ModelAndApp
    (ToolUi(uri.flatMap(_.toOption), vis), Chunk.fromIterable(uri.flatMap(_.left.toOption)))
  end decodeTool

  /** `McpUiResourceCsp`: each directive's origins, sorted; an empty directive is omitted. */
  def encodeCsp(n: Network): Json.Obj =
    def list(d: Directive, os: Set[Origin]) =
      Option.when(os.nonEmpty)(d.wire -> Json.Arr(Chunk.fromIterable(os.toList.map(_.render).sorted).map(Json.Str(_))))
    Json.Obj(
      Chunk(
        list(Directive.Connect, n.connect),
        list(Directive.Resource, n.resources),
        list(Directive.Frame, n.frames),
        list(Directive.Base, n.base),
      ).flatten
    )
  end encodeCsp

  /** `McpUiResourcePermissions`: one empty object per permission. */
  def encodePermissions(ps: Set[Permission]): Json.Obj =
    Json.Obj(Chunk.fromIterable(ps.toList.sortBy(_.ordinal)).map(p => p.wire -> Json.Obj()))

  def decodeCsp(csp: Json.Obj): (Network, Chunk[MetaProblem]) =
    def origins(d: Directive): (Set[Origin], Chunk[MetaProblem]) =
      val raws =
        csp.get(d.wire).collect { case Json.Arr(vs) => vs.collect { case Json.Str(s) => s } }.getOrElse(Chunk.empty)
      val parsed = raws.map(r => Origin.from(r).left.map(MetaProblem.BadOrigin(d, r, _)))
      (parsed.flatMap(_.toOption).toSet, parsed.flatMap(_.left.toOption))
    val (connect, p1)   = origins(Directive.Connect)
    val (resources, p2) = origins(Directive.Resource)
    val (frames, p3)    = origins(Directive.Frame)
    val (base, p4)      = origins(Directive.Base)
    (Network(connect, resources, frames, base), p1 ++ p2 ++ p3 ++ p4)
  end decodeCsp

  def decodePermissions(perms: Json.Obj): (Set[Permission], Chunk[MetaProblem]) =
    val asked = perms.fields.map(_._1)
    (
      asked.flatMap(n => Permission.values.find(_.wire == n)).toSet,
      asked.filterNot(n => Permission.values.exists(_.wire == n)).map(MetaProblem.UnknownPermission(_)),
    )

  def encodeResource(policy: UiPolicy): Json.Obj =
    val csp    = encodeCsp(policy.network)
    val perm   = encodePermissions(policy.permissions)
    val fields = Chunk.fromIterable(Option.when(csp.fields.nonEmpty)("csp" -> csp)) ++
      Chunk.fromIterable(Option.when(perm.fields.nonEmpty)("permissions" -> perm)) ++
      Chunk.fromIterable(policy.origin match
        case AppOrigin.Stable(label) => Some("domain" -> Json.Str(label))
        case AppOrigin.Opaque        => None) ++
      Chunk.fromIterable(policy.border match
        case Border.Visible     => Some("prefersBorder" -> Json.Bool(true))
        case Border.Hidden      => Some("prefersBorder" -> Json.Bool(false))
        case Border.HostDefault => None)
    Json.Obj(Key -> Json.Obj(fields))
  end encodeResource

  def decodeResource(meta: Option[Json.Obj]): (UiPolicy, Chunk[MetaProblem]) =
    val ui                     = meta.flatMap(_.get(Key)).collect { case o: Json.Obj => o }.getOrElse(Json.Obj())
    val (network, cspProblems) = decodeCsp(ui.get("csp").collect { case o: Json.Obj => o }.getOrElse(Json.Obj()))
    val (known, unknown)       =
      decodePermissions(ui.get("permissions").collect { case o: Json.Obj => o }.getOrElse(Json.Obj()))
    val origin = ui
      .get("domain")
      .collect { case Json.Str(s) if s.nonEmpty => AppOrigin.Stable(s) }
      .getOrElse(
        AppOrigin.Opaque
      )
    val border = ui.get("prefersBorder") match
      case Some(Json.Bool(true))  => Border.Visible
      case Some(Json.Bool(false)) => Border.Hidden
      case _                      => Border.HostDefault
    (UiPolicy(network, known, origin, border), cspProblems ++ unknown)
  end decodeResource
end UiMeta
