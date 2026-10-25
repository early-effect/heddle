package heddle.docs.ui

import heddle.docs.fixture.{BoxOffice, Show}
import heddle.endpoint.Schema
import heddle.http.Url
import heddle.mcp.McpBuildError
import heddle.mcp.protocol.ClientRequest
import zio.*
import zio.json.*
import zio.json.ast.Json

/** JSON, OpenAPI, and MCP readings of `Show`, taken from the bound box office rather than copied into the page. */
final case class SchemaPicture(
    json: String,
    openApi: String,
    openApiWhere: String,
    mcpArgs: String,
    mcpWhere: String,
)

object SchemaPicture:

  enum Gap:
    case EmptyHouse
    case ShowNotServed(status: String)
    case NoBody
    case UnnamedShow
    case OpenApiUnreadable(detail: String)
    case NoShowComponent(name: String)
    case McpBuild(errors: NonEmptyChunk[McpBuildError])
    case NoReply
    case NoInputSchema(name: String)

  def load: IO[Gap, SchemaPicture] =
    for
      store <- BoxOffice.seed
      shows <- store.get
      show  <- shows.values.minByOption(_.id) match
        case Some(found) => ZIO.succeed(found)
        case None        => ZIO.fail(Gap.EmptyHouse)
      api = BoxOffice.api(store)
      response <- api.routes(BoxOffice.getShow.toRequest(show.id, Url.root)).mapError { err =>
        Gap.ShowNotServed(err.status.render)
      }
      json <- (response.status.isSuccess, response.body.text) match
        case (true, Some(text)) => ZIO.succeed(text)
        case (false, _)         => ZIO.fail(Gap.ShowNotServed(response.status.render))
        case (true, None)       => ZIO.fail(Gap.NoBody)
      title <- summon[Schema[Show]].named match
        case Some(name) => ZIO.succeed(name)
        case None       => ZIO.fail(Gap.UnnamedShow)
      root      <- ZIO.fromEither(api.openApi.toJson.fromJson[Json]).mapError(Gap.OpenApiUnreadable(_))
      component <- component(root, title) match
        case Some(schema) => ZIO.succeed(schema)
        case None         => ZIO.fail(Gap.NoShowComponent(title))
      tool = BoxOffice.getShow.doc.toolName
      mcp    <- ZIO.fromEither(BoxOffice.mcpOf(store)).mapError(Gap.McpBuild(_))
      listed <- mcp.handle(BoxOffice.rpc(ClientRequest.ListTools(None)))
      reply  <- listed match
        case Some(value) => ZIO.succeed(value)
        case None        => ZIO.fail(Gap.NoReply)
      input <- inputSchema(reply, tool) match
        case Some(schema) => ZIO.succeed(schema)
        case None         => ZIO.fail(Gap.NoInputSchema(tool))
    yield SchemaPicture(
      json = json,
      openApi = HarnessScript.pretty(component),
      openApiWhere = s"components.schemas.$title",
      mcpArgs = HarnessScript.pretty(input),
      mcpWhere = s"inputSchema for $tool",
    )

  /** Another object in the generated transcript file. The package clause is already written. */
  def source(picture: SchemaPicture): String =
    "object SchemaTranscript:\n" +
      "  /** Generated from the box office Api by WriteTranscript. Do not edit. */\n" +
      "  val json: String =\n    " + ScalaLiteral(picture.json) + "\n" +
      "  val openApi: String =\n    " + ScalaLiteral(picture.openApi) + "\n" +
      "  val openApiWhere: String =\n    " + ScalaLiteral(picture.openApiWhere) + "\n" +
      "  val mcpArgs: String =\n    " + ScalaLiteral(picture.mcpArgs) + "\n" +
      "  val mcpWhere: String =\n    " + ScalaLiteral(picture.mcpWhere) + "\n"

  private def component(root: Json, name: String): Option[Json] =
    child(root, "components").flatMap(child(_, "schemas")).flatMap(child(_, name))

  private def inputSchema(message: Json, tool: String): Option[Json] =
    child(message, "result").flatMap(child(_, "tools")).flatMap {
      case Json.Arr(tools) =>
        tools.collectFirst {
          case entry: Json.Obj if entry.get("name").contains(Json.Str(tool)) => entry.get("inputSchema")
        }.flatten
      case _ => None
    }

  private def child(json: Json, key: String): Option[Json] =
    json match
      case obj: Json.Obj => obj.get(key)
      case _             => None
end SchemaPicture
