package heddle.mcp

import heddle.LinePipe
import heddle.endpoint.{Api, BoundOp, OpArgs, Schema, SchemaJson}
import heddle.http.Response
import heddle.http.header.{AuthScheme, Authorization, Headers}
import heddle.http.header.Authorization.given
import heddle.mcp.auth.ProtectedResource
import heddle.mcp.protocol.{Era, ExtensionId, Implementation, Structured, Tool, ToolName}
import heddle.mcp.server.{Engine, Offer, ToolCall}
import heddle.mcp.transport.{Http, Stdio}
import heddle.route.Routes
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, NonEmptyChunk, ZIO, ZNothing}

import java.io.{InputStream, OutputStream}

/** An MCP server over bound operations, native tools, and resources. Serve it on HTTP (`routes`) or a pipe (`stdio`).
  * Everything that adds to what it offers is checked: a second tool or resource with a taken name is a build error.
  */
final class Mcp[-R] private (
    val server: Implementation,
    offer: Offer[R],
    catalogOps: Chunk[BoundOp[R, ?, ?, ?]],
    instructions: Option[String],
    path: String,
):
  def serverName: String    = server.name
  def serverVersion: String = server.version

  /** Adds `search_operations` and `invoke`, which reach every promotable operation, promoted or not. */
  def withCatalog: Either[NonEmptyChunk[McpBuildError], Mcp[R]] =
    Mcp.catalogTools(catalogOps).foldLeft[Either[NonEmptyChunk[McpBuildError], Mcp[R]]](Right(this)) { (acc, t) =>
      acc.flatMap(_.withTool(t))
    }

  def instructions(text: String): Mcp[R] = copy(instructions = Some(text))

  def at(path: String): Mcp[R] = copy(path = path.stripPrefix("/"))

  /** A tool that is not an HTTP operation: `mcp.tool[Query]("search_users", "Find users") { q => ... }`. The name is
    * checked at compile time and `A` is pinned, so the function needs no ascription. Arguments are `A` (as
    * `{"value": a}` when `A` is not an object). The function's error type is the tool's own error (see [[ToolError]]).
    */
  inline def tool[A](inline name: String, description: String = ""): Mcp.ToolBuilder[R, A] =
    Mcp.ToolBuilder(this, ToolName(name), description)

  def withTool[R1](t: ToolCall[R1]): Either[NonEmptyChunk[McpBuildError], Mcp[R & R1]] =
    if offer.tools.exists(_.tool.name == t.tool.name) then Left(NonEmptyChunk(McpBuildError.DuplicateTool(t.tool.name)))
    else Right(new Mcp(server, offer.copy(tools = offer.tools :+ t), catalogOps, instructions, path))

  /** Resources clients list and read. A taken URI, here or already on the server, is a build error. */
  def withResources[R1](served: ServedResource[R1]*): Either[NonEmptyChunk[McpBuildError], Mcp[R & R1]] =
    val uris  = offer.resources.map(_.resource.uri) ++ served.map(_.resource.uri)
    val dupes = uris.diff(uris.distinct).distinct.map(McpBuildError.DuplicateResource(_))
    NonEmptyChunk.fromChunk(dupes) match
      case Some(errors) => Left(errors)
      case None         =>
        Right(new Mcp(server, offer.copy(resources = offer.resources ++ served), catalogOps, instructions, path))

  /** Gives the tool named `name` extra `_meta` (merged by key). A promotable operation that is not listed yet, such as
    * one only an App's view calls, is listed now. A name no operation has is a build error.
    */
  def withToolMeta(name: ToolName, meta: Json.Obj): Either[NonEmptyChunk[McpBuildError], Mcp[R]] =
    def merged(t: ToolCall[R]): ToolCall[R] =
      val base = t.tool.meta.getOrElse(Json.Obj())
      val next = Json.Obj(base.fields.filterNot((k, _) => meta.fields.exists(_._1 == k)) ++ meta.fields)
      ToolCall(t.tool.copy(meta = Some(next)))(t.call)
    offer.tools.indexWhere(_.tool.name == name) match
      case -1 =>
        catalogOps.find(_.endpoint.doc.toolName == name.value) match
          case None     => Left(NonEmptyChunk(McpBuildError.NoSuchTool(name)))
          case Some(op) =>
            ToolCall.bound(op).left.map(NonEmptyChunk(_)).map { t =>
              new Mcp(server, offer.copy(tools = offer.tools :+ merged(t)), catalogOps, instructions, path)
            }
      case i =>
        Right(
          new Mcp(
            server,
            offer.copy(tools = offer.tools.updated(i, merged(offer.tools(i)))),
            catalogOps,
            instructions,
            path,
          )
        )
    end match
  end withToolMeta

  /** Advertises an extension in `server/discover` and `initialize` capabilities, with its settings. */
  def withExtension(id: ExtensionId, settings: Json.Obj = Json.Obj()): Mcp[R] =
    new Mcp(server, offer.copy(extensions = offer.extensions.updated(id, settings)), catalogOps, instructions, path)

  def routes: Routes[R, ZNothing] =
    Http.routes(engine, path)

  def discovery(
      resource: String,
      authorizationServers: List[String],
      scopes: List[String] = Nil,
  ): Routes[Any, Nothing] =
    ProtectedResource.routes(resource, authorizationServers, scopes)

  def stdio(): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, LinePipe.standard)

  def stdio(pipe: LinePipe): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, pipe)

  def stdio(in: InputStream, out: OutputStream): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, LinePipe.streams(in, out))

  /** Answers one 2026-07-28 JSON-RPC value, as `POST /mcp` would. */
  def handle(json: Json, headers: Headers = Headers.empty): ZIO[R, Nothing, Option[Json]] =
    engine.handle(json, headers, Era.Stateless).map(_.map(_.json))

  private def engine: Engine[R] =
    Engine(server, offer, instructions, 300000)

  private def copy(instructions: Option[String] = instructions, path: String = path): Mcp[R] =
    new Mcp(server, offer, catalogOps, instructions, path)
end Mcp

object Mcp:
  /** Every promoted operation becomes a tool, or every reason it cannot is returned. */
  def from[R](apis: Api[R]*): Either[NonEmptyChunk[McpBuildError], Mcp[R]] =
    val ops      = Chunk.fromIterable(apis).flatMap(_.ops)
    val promoted = ops.filter(o => o.endpoint.doc.promoted && OpArgs.promotable(o.endpoint.doc))
    val catalog  = ops.filter(o => OpArgs.promotable(o.endpoint.doc))
    val built    = promoted.map(ToolCall.bound)
    val tools    = built.collect { case Right(t) => t }
    val names    = tools.map(_.tool.name)
    val dupes    = names.diff(names.distinct).distinct.map(McpBuildError.DuplicateTool(_))
    NonEmptyChunk.fromChunk(built.collect { case Left(e) => e } ++ dupes) match
      case Some(errors) => Left(errors)
      case None         =>
        val title   = apis.headOption.map(_.title).getOrElse("heddle")
        val version = apis.headOption.map(_.version).getOrElse("0.0.0")
        Right(new Mcp(Implementation(title, version), Offer(tools, Chunk.empty, Map.empty), catalog, None, "mcp"))
  end from

  def unauthorized(resourceMetadata: String, scopes: List[String] = Nil): Response =
    ProtectedResource.unauthorized(resourceMetadata, scopes)

  def bearer[R, P](resourceMetadata: String, scopes: List[String] = Nil)(
      validate: String => ZIO[R, Response, P]
  ): heddle.http.Request => ZIO[R, Response, P] =
    req =>
      req.headers.get[Authorization] match
        case Some(Authorization(AuthScheme.Bearer, token)) if token.nonEmpty =>
          validate(token)
        case _ =>
          ZIO.fail(unauthorized(resourceMetadata, scopes))

  def protectedResource(
      resource: String,
      authorizationServers: List[String],
      scopes: List[String] = Nil,
  ): Routes[Any, Nothing] =
    ProtectedResource.routes(resource, authorizationServers, scopes)

  final class ToolBuilder[-R, A](mcp: Mcp[R], name: ToolName, description: String):
    def apply[R1, E, B](f: A => ZIO[R1, E, B])(using
        Schema[A],
        JsonCodec[A],
        ToolError[E],
        Schema[B],
        JsonCodec[B],
    ): Either[NonEmptyChunk[McpBuildError], Mcp[R & R1]] =
      mcp.withTool(native(name, description, f))

  def native[R, A, E, B](name: ToolName, description: String, f: A => ZIO[R, E, B])(using
      sa: Schema[A],
      ja: JsonCodec[A],
      te: ToolError[E],
      sb: Schema[B],
      jb: JsonCodec[B],
  ): ToolCall[R] =
    val in   = Structured.of(SchemaJson.render(sa.doc))
    val out  = Structured.of(SchemaJson.render(sb.doc))
    val tool = Tool(
      name,
      description = Option.when(description.nonEmpty)(description),
      inputSchema = in.outputSchema,
      outputSchema = Some(out.outputSchema),
    )
    ToolCall(tool) { (args, _) =>
      in.unwrap(args).as[A](using ja.decoder) match
        case Left(msg) => ZIO.succeed(ToolCall.failed(msg))
        case Right(a)  =>
          f(a).fold(
            e => ToolCall.typedFailure(te.json(e), te.shape),
            b => ToolCall.success(jb.encoder.toJsonAST(b).getOrElse(Json.Null), Some(out)),
          )
    }
  end native

  private def catalogTools[R](ops: Chunk[BoundOp[R, ?, ?, ?]]): Chunk[ToolCall[R]] =
    val byName = ops.map(o => o.endpoint.doc.toolName -> o).toMap
    Chunk(searchTool(ops), invokeTool(byName))

  private def searchTool[R](ops: Chunk[BoundOp[R, ?, ?, ?]]): ToolCall[R] =
    val schema = Json.Obj(
      "type"       -> Json.Str("object"),
      "properties" -> Json.Obj("query" -> Json.Obj("type" -> Json.Str("string"))),
      "required"   -> Json.Arr(Json.Str("query")),
    )
    val tool =
      Tool(
        ToolName("search_operations"),
        description = Some("Search API operations by name, path, or summary"),
        inputSchema = schema,
      )
    ToolCall(tool) { (args, _) =>
      val q    = args.get("query").collect { case Json.Str(s) => s.toLowerCase }.getOrElse("")
      val hits = ops.filter { op =>
        val d    = op.endpoint.doc
        val blob =
          s"${d.toolName} ${d.pathTemplate} ${d.summary.getOrElse("")} ${d.description.getOrElse("")}".toLowerCase
        q.isEmpty || blob.contains(q)
      }
      val found = Json.Arr(hits.map { op =>
        val d = op.endpoint.doc
        Json.Obj(
          "name"    -> Json.Str(d.toolName),
          "method"  -> Json.Str(d.method.render),
          "path"    -> Json.Str(d.pathTemplate),
          "summary" -> Json.Str(d.summary.getOrElse("")),
        )
      })
      ZIO.succeed(ToolCall.success(Json.Obj("operations" -> found), None))
    }
  end searchTool

  private def invokeTool[R](byName: Map[String, BoundOp[R, ?, ?, ?]]): ToolCall[R] =
    val schema = Json.Obj(
      "type"       -> Json.Str("object"),
      "properties" -> Json.Obj(
        "operationId" -> Json.Obj("type" -> Json.Str("string")),
        "arguments"   -> Json.Obj("type" -> Json.Str("object")),
      ),
      "required" -> Json.Arr(Json.Str("operationId")),
    )
    val tool = Tool(ToolName("invoke"), description = Some("Invoke an API operation by name"), inputSchema = schema)
    ToolCall(tool) { (args, headers) =>
      val inner = args.get("arguments").collect { case o: Json.Obj => o }.getOrElse(Json.Obj())
      args.get("operationId").collect { case Json.Str(s) => s } match
        case None     => ZIO.succeed(ToolCall.failed("missing operationId"))
        case Some(id) =>
          byName.get(id) match
            case None     => ZIO.succeed(ToolCall.failed(s"Unknown operation: $id"))
            case Some(op) =>
              ToolCall.bound(op) match
                case Left(e)  => ZIO.succeed(ToolCall.failed(e.message))
                case Right(t) => t.call(inner, headers)
    }
  end invokeTool
end Mcp
