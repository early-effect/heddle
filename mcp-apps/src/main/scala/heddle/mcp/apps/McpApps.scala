package heddle.mcp.apps

import heddle.mcp.{Mcp, McpBuildError, ServedResource}
import heddle.mcp.protocol.{ExtensionId, Resource, ResourceContents, ToolName, ToolNameError}
import zio.json.ast.Json
import zio.{Chunk, NonEmptyChunk, ZIO}

/** Why a shed could not be served. */
enum AppBuildError(val message: String) extends heddle.error.HeddleError:
  case Mcp(error: McpBuildError) extends AppBuildError(error.message)
  case BadToolName(grant: String, reason: ToolNameError)
      extends AppBuildError(s"$grant cannot be an MCP tool name: ${reason.message}")
  case LaunchNotForModel(grant: String)
      extends AppBuildError(s"$grant opens the view, so the model must be able to call it")

/** `_meta` key for Heddle's own additions to a view resource: the inline script's hash, for a host to pin in CSP. */
val HeddleMetaKey = "rocks.earlyeffect/heddle"

extension [R](mcp: Mcp[R])
  /** Serves `shed`'s view as a `ui://` resource and marks every granted tool with `_meta.ui`. Tools only the view calls
    * are listed now, with `visibility: ["app"]`, so hosts can route the view's calls and keep them from the model.
    * Advertises the MCP Apps extension.
    */
  def withApp(shed: Shed[?, ?], document: UiDocument): Either[NonEmptyChunk[AppBuildError], Mcp[R]] =
    val html   = document.html
    val uiMeta = UiMeta.encodeResource(shed.policy)
    val meta   =
      Json.Obj(uiMeta.fields :+ (HeddleMetaKey -> Json.Obj("scriptHashes" -> Json.Arr(Json.Str(document.scriptHash)))))
    val resource = Resource(shed.uri.value, shed.title, mimeType = Some(UiMeta.MimeType), meta = Some(meta))
    val served   = ServedResource(
      resource,
      ZIO.succeed(Chunk(ResourceContents.Text(shed.uri.value, Some(UiMeta.MimeType), html, Some(meta)))),
    )
    val launchOk =
      if shed.launch.visibility.model then Right(())
      else Left(NonEmptyChunk(AppBuildError.LaunchNotForModel(shed.launch.toolName)))
    for
      _      <- launchOk
      names  <- named(shed.grants)
      marked <- names.foldLeft[Either[NonEmptyChunk[AppBuildError], Mcp[R]]](Right(mcp)) { case (acc, (name, g)) =>
        acc.flatMap(
          _.withToolMeta(name, UiMeta.encodeTool(ToolUi(Some(shed.uri), g.visibility))).left
            .map(_.map(AppBuildError.Mcp(_)))
        )
      }
      withView <- marked.withResources(served).left.map(_.map(AppBuildError.Mcp(_)))
    yield withView.withExtension(ExtensionId.Ui, Json.Obj("mimeTypes" -> Json.Arr(Json.Str(UiMeta.MimeType))))
    end for
  end withApp
end extension

private def named(
    grants: List[Grant[?, ?, ?]]
): Either[NonEmptyChunk[AppBuildError], List[(ToolName, Grant[?, ?, ?])]] =
  val parsed = grants.map(g => ToolName.from(g.toolName).left.map(AppBuildError.BadToolName(g.toolName, _)).map(_ -> g))
  NonEmptyChunk.fromIterableOption(parsed.collect { case Left(e) => e }) match
    case Some(errors) => Left(errors)
    case None         => Right(parsed.collect { case Right(p) => p })
