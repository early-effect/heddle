package heddle.mcp

import heddle.LinePipe
import heddle.endpoint.{Api, BoundOp, OpArgs, Schema, SchemaJson}
import heddle.http.Response
import heddle.http.header.{AuthScheme, Authorization, Headers}
import heddle.http.header.Authorization.given
import heddle.mcp.auth.ProtectedResource
import heddle.mcp.protocol.{Implementation, Structured, Tool, ToolName}
import heddle.mcp.server.{Engine, Era, ToolCall}
import heddle.mcp.transport.{Http, Stdio}
import heddle.route.Routes
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, NonEmptyChunk, ZIO, ZNothing}

import java.io.{InputStream, OutputStream}

/** An MCP server over bound operations and native tools. Serve it on HTTP (`routes`) or a pipe (`stdio`). */
final class Mcp[-R] private (
    val server: Implementation,
    promoted: Chunk[ToolCall[R]],
    catalogOps: Chunk[BoundOp[R, ?, ?, ?]],
    catalogOn: Boolean,
    instructions: Option[String],
    path: String,
):
  def serverName: String    = server.name
  def serverVersion: String = server.version

  def withCatalog: Mcp[R] = copy(catalogOn = true)

  def instructions(text: String): Mcp[R] = copy(instructions = Some(text))

  def at(path: String): Mcp[R] = copy(path = path.stripPrefix("/"))

  /** A tool that is not an HTTP operation: `mcp.tool[Query]("search_users", "Find users") { q => ... }`. The name is
    * checked at compile time and `A` is pinned, so the function needs no ascription. Arguments are `A` (as
    * `{"value": a}` when `A` is not an object). The function's error type is the tool's own error (see [[ToolError]]).
    */
  inline def tool[A](inline name: String, description: String = ""): Mcp.ToolBuilder[R, A] =
    Mcp.ToolBuilder(this, ToolName(name), description)

  def withTool[R1](t: ToolCall[R1]): Mcp[R & R1] =
    new Mcp(server, promoted :+ t, catalogOps, catalogOn, instructions, path)

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
    val extra = if catalogOn then Mcp.catalogTools(catalogOps) else Chunk.empty
    Engine(server, promoted ++ extra, instructions, 300000)

  private def copy[R1 <: R](
      catalogOn: Boolean = catalogOn,
      instructions: Option[String] = instructions,
      path: String = path,
  ): Mcp[R1] =
    new Mcp(server, promoted, catalogOps, catalogOn, instructions, path)
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
        Right(new Mcp(Implementation(title, version), tools, catalog, false, None, "mcp"))
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
    ): Mcp[R & R1] =
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
