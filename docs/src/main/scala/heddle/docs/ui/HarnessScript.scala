package heddle.docs.ui

import heddle.docs.fixture.BoxOffice
import heddle.endpoint.{EndpointDoc, Hint, OpArgs, SchemaDoc}
import heddle.http.{Request, Url}
import heddle.mcp.McpBuildError
import heddle.mcp.protocol.{ClientRequest, ToolName, ToolNameError}
import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json

/** The harness and catalog pictures, rendered from a bound `Api` rather than copied into the page. */
final case class HarnessScript(
    list: String,
    call: String,
    catalogPromoted: String,
    catalogFull: String,
    rpcRequest: String,
    rpcResponse: String,
    rpcKicker: String,
)

object HarnessScript:

  /** Tools `Mcp.withCatalog` adds. They are not operations. */
  val catalogTools: List[String] = List("search_operations", "invoke")

  enum Gap:
    case EmptyHouse
    case MissingTool(name: String)
    case PathArgMissing(name: String)
    case ShowNotServed(status: String)
    case NoBody
    case BadToolName(error: ToolNameError)
    case McpBuild(errors: NonEmptyChunk[McpBuildError])
    case NoReply

  def load: IO[Gap, HarnessScript] =
    for
      store <- BoxOffice.seed
      shows <- store.get
      show  <- shows.values.minByOption(_.id) match
        case Some(found) => ZIO.succeed(found)
        case None        => ZIO.fail(Gap.EmptyHouse)
      api  = BoxOffice.api(store)
      docs = api.ops.map(_.endpoint.doc).toList
      tool <- docs.find(_.toolName == BoxOffice.getShow.doc.toolName) match
        case Some(found) => ZIO.succeed(found)
        case None        => ZIO.fail(Gap.MissingTool(BoxOffice.getShow.doc.toolName))
      request = BoxOffice.getShow.toRequest(show.id, Url.root)
      response <- api.routes(request).mapError(err => Gap.ShowNotServed(err.status.render))
      json     <- (response.status.isSuccess, response.body.text) match
        case (true, Some(text)) => ZIO.succeed(text)
        case (false, _)         => ZIO.fail(Gap.ShowNotServed(response.status.render))
        case (true, None)       => ZIO.fail(Gap.NoBody)
      name <- ToolName.from(tool.toolName) match
        case Right(found) => ZIO.succeed(found)
        case Left(error)  => ZIO.fail(Gap.BadToolName(error))
      args <- ZIO.fromEither(callArguments(tool, request))
      wire = BoxOffice.rpc(ClientRequest.CallTool(name, args))
      mcp      <- ZIO.fromEither(BoxOffice.mcpOf(store)).mapError(Gap.McpBuild(_))
      answered <- mcp.handle(wire)
      reply    <- answered match
        case Some(value) => ZIO.succeed(value)
        case None        => ZIO.fail(Gap.NoReply)
    yield render(docs, tool, request, json, wire, reply)

  /** Scala source of the strings the shared page reads. JVM and Scala.js both compile this. */
  def source(script: HarnessScript): String =
    "package heddle.docs.ui\n\n" +
      "/** Generated from the box office Api by WriteTranscript. Do not edit. */\n" +
      "object HarnessTranscript:\n" +
      "  val list: String =\n    " + ScalaLiteral(script.list) + "\n" +
      "  val call: String =\n    " + ScalaLiteral(script.call) + "\n" +
      "  val catalogPromoted: String =\n    " + ScalaLiteral(script.catalogPromoted) + "\n" +
      "  val catalogFull: String =\n    " + ScalaLiteral(script.catalogFull) + "\n" +
      "  val rpcRequest: String =\n    " + ScalaLiteral(script.rpcRequest) + "\n" +
      "  val rpcResponse: String =\n    " + ScalaLiteral(script.rpcResponse) + "\n" +
      "  val rpcKicker: String =\n    " + ScalaLiteral(script.rpcKicker) + "\n"

  /** Two-space indent, field order left as the codecs wrote it. */
  def pretty(json: Json): String = renderJson(json, 0)

  private def render(
      docs: List[EndpointDoc],
      tool: EndpointDoc,
      request: Request,
      json: String,
      wire: Json,
      reply: Json,
  ): HarnessScript =
    val promoted             = docs.filter(doc => doc.promoted && OpArgs.promotable(doc))
    val hidden               = docs.filterNot(doc => doc.promoted && OpArgs.promotable(doc))
    val (reachable, blocked) = hidden.partition(OpArgs.promotable)
    val rows                 = align(promoted)
    val footnote             = note(reachable.map(_.toolName), blocked.map(_.toolName))
    HarnessScript(
      list = listText(rows, footnote),
      call = callText(tool, request, json),
      catalogPromoted = catalogPromotedText(rows, footnote),
      catalogFull = catalogFullText(rows, reachable.map(_.toolName)),
      rpcRequest = pretty(wire),
      rpcResponse = pretty(reply),
      rpcKicker = s"same function as ${request.method.render} ${request.url.path.render}",
    )
  end render

  private def listText(rows: List[String], footnote: String): String =
    val body = dialogue(rows)
    if footnote.isEmpty then s"Grok  →  tools/list\n$body" else s"Grok  →  tools/list\n$body\n\n$footnote"

  private def catalogPromotedText(rows: List[String], footnote: String): String =
    val bullets = rows.map(row => s"- $row").mkString("\n")
    if footnote.isEmpty then s"tools/list  (promoted only)\n$bullets"
    else s"tools/list  (promoted only)\n$bullets\n\n$footnote"

  private def catalogFullText(rows: List[String], unpromoted: List[String]): String =
    val bullets = rows.map(row => s"- $row").mkString("\n")
    val extras  = catalogTools
      .map { name =>
        if name == "invoke" && unpromoted.nonEmpty then s"- invoke            # ${unpromoted.mkString(", ")} lives here"
        else s"- $name"
      }
      .mkString("\n")
    s"tools/list  (withCatalog)\n$bullets\n$extras"

  private def callText(doc: EndpointDoc, request: Request, json: String): String =
    val target = s"${request.method.render} ${request.url.path.render}"
    s"Grok  →  tools/call  ${doc.toolName}  ${argsOf(doc, request)}\n" +
      s"hub   ←  $json\n\n" +
      s"Same function as $target."

  private def dialogue(rows: List[String]): String =
    val lead = "hub   ←  "
    rows match
      case Nil           => s"$lead(none)"
      case first :: rest =>
        val more = rest.map(row => s"${" " * lead.length}$row")
        (s"$lead$first" :: more).mkString("\n")

  private def align(docs: List[EndpointDoc]): List[String] =
    val nameW   = widest(docs.map(_.toolName))
    val methodW = widest(docs.map(_.method.render))
    val pathW   = widest(docs.map(_.pathTemplate))
    docs.map { doc =>
      val hints = doc.hints.map(hintLabel).mkString(" ")
      val hint  = if hints.isEmpty then "" else s"  $hints"
      s"${doc.toolName.padTo(nameW, ' ')}  ${doc.method.render.padTo(methodW, ' ')} ${doc.pathTemplate.padTo(pathW, ' ')}$hint".stripTrailing
    }

  private def note(unpromoted: List[String], blocked: List[String]): String =
    val open = unpromoted match
      case Nil         => ""
      case name :: Nil => s"$name is not promoted. Catalog can still find it."
      case names       => s"${names.mkString(", ")} are not promoted. Catalog can still find them."
    val shut = blocked match
      case Nil         => ""
      case name :: Nil => s"$name cannot be a tool."
      case names       => s"${names.mkString(", ")} cannot be tools."
    List(open, shut).filter(_.nonEmpty).mkString("\n")
  end note

  private def argsOf(doc: EndpointDoc, request: Request): String =
    val names    = doc.pathTemplate.split('/').toList.filter(_.nonEmpty)
    val pathArgs = names.zip(request.url.path.segments.toList).collect {
      case (raw, value) if raw.startsWith("{") && raw.endsWith("}") && raw.length >= 2 =>
        s"${raw.substring(1, raw.length - 1)}: $value"
    }
    val queryArgs = doc.queries.flatMap { query =>
      request.url.query.getAll(query.name).toList.map(value => s"${query.name}: $value")
    }
    s"{${(pathArgs ++ queryArgs).mkString(", ")}}"
  end argsOf

  private def callArguments(doc: EndpointDoc, request: Request): Either[Gap, Json.Obj] =
    val names  = doc.pathTemplate.split('/').toList.filter(_.nonEmpty)
    val filled = names
      .zip(request.url.path.segments.toList)
      .collect {
        case (raw, value) if raw.startsWith("{") && raw.endsWith("}") && raw.length >= 2 =>
          raw.substring(1, raw.length - 1) -> value
      }
      .toMap
    val pathFields = doc.pathParams.foldLeft[Either[Gap, List[(String, Json)]]](Right(Nil)) { (acc, param) =>
      acc.flatMap { fields =>
        filled.get(param.name) match
          case Some(raw) => Right(fields :+ (param.name -> jsonValue(param.schema, raw)))
          case None      => Left(Gap.PathArgMissing(param.name))
      }
    }
    pathFields.map { fields =>
      val queryFields = doc.queries.flatMap { param =>
        request.url.query.getAll(param.name).toList.map(value => param.name -> jsonValue(param.schema, value))
      }
      Json.Obj(Chunk.fromIterable(fields ++ queryFields))
    }
  end callArguments

  private def jsonValue(schema: SchemaDoc, raw: String): Json =
    schema.unwrapOptional._1 match
      case SchemaDoc.Integer(_) =>
        raw.toIntOption match
          case Some(n) => Json.Num(n)
          case None    => Json.Str(raw)
      case SchemaDoc.Number(_) =>
        raw.toDoubleOption match
          case Some(n) => Json.Num(n)
          case None    => Json.Str(raw)
      case SchemaDoc.Boolean =>
        raw match
          case "true"  => Json.Bool(true)
          case "false" => Json.Bool(false)
          case _       => Json.Str(raw)
      case _ => Json.Str(raw)

  private def renderJson(json: Json, indent: Int): String =
    val pad  = "  " * indent
    val nest = "  " * (indent + 1)
    json match
      case Json.Obj(fields) if fields.isEmpty => "{}"
      case Json.Obj(fields)                   =>
        val body = fields
          .map { (key, value) =>
            s"$nest${Json.Str(key).toJson}: ${renderJson(value, indent + 1)}"
          }
          .mkString(",\n")
        s"{\n$body\n$pad}"
      case Json.Arr(values) if values.isEmpty => "[]"
      case Json.Arr(values)                   =>
        val body = values.map(value => s"$nest${renderJson(value, indent + 1)}").mkString(",\n")
        s"[\n$body\n$pad]"
      case other => other.toJson
    end match
  end renderJson

  private def hintLabel(hint: Hint): String =
    hint match
      case Hint.ReadOnly    => "ReadOnly"
      case Hint.Destructive => "Destructive"
      case Hint.Idempotent  => "Idempotent"
      case Hint.OpenWorld   => "OpenWorld"

  private def widest(values: List[String]): Int =
    values.map(_.length).maxOption.getOrElse(0)
end HarnessScript
