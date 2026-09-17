package heddle.mcp

import heddle.endpoint.{Api, BoundOp, OpArgs, Schema, SchemaJson}
import heddle.http.Response
import heddle.http.header.{AuthScheme, Authorization, Headers}
import heddle.http.header.Authorization.given
import heddle.mcp.auth.ProtectedResource
import heddle.mcp.protocol.{CallResult, Engine, ToolCall, ToolDef}
import heddle.mcp.protocol.JsonRpc
import heddle.mcp.protocol.JsonRpc.*
import heddle.mcp.transport.{Http, Stdio}
import heddle.route.Routes
import zio.json.{DecoderOps, EncoderOps, JsonCodec}
import zio.json.ast.Json
import zio.{Chunk, ZIO, ZNothing}

import java.io.{InputStream, OutputStream}

final class Mcp[-R] private (
    val serverName: String,
    val serverVersion: String,
    promoted: Chunk[ToolCall[R]],
    catalogOps: Chunk[BoundOp[R, ?, ?, ?]],
    catalogOn: Boolean,
    instructions: Option[String],
    path: String,
):
  def withCatalog: Mcp[R] = copy(catalogOn = true)

  def instructions(text: String): Mcp[R] = copy(instructions = Some(text))

  def at(path: String): Mcp[R] = copy(path = path.stripPrefix("/"))

  def tool[R1, A, B](name: String, description: String = "")(
      f: A => ZIO[R1, String, B]
  )(using Schema[A], JsonCodec[A], Schema[B], JsonCodec[B]): Mcp[R & R1] =
    new Mcp(
      serverName,
      serverVersion,
      promoted ++ Chunk(Mcp.native(name, description, f)),
      catalogOps,
      catalogOn,
      instructions,
      path,
    )

  def routes: Routes[R, ZNothing] =
    Http.routes(engine, path)

  def discovery(
      resource: String,
      authorizationServers: List[String],
      scopes: List[String] = Nil,
  ): Routes[Any, Nothing] =
    ProtectedResource.routes(resource, authorizationServers, scopes)

  def stdio(in: InputStream = System.in, out: OutputStream = System.out): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, in, out)

  def handle(json: Json, headers: Headers = Headers.empty): ZIO[R, Nothing, Option[Json]] =
    engine.handle(json, headers).map(_.map(o => o: Json))

  private def engine: Engine[R] =
    val extra = if catalogOn then Mcp.catalogTools(catalogOps) else Chunk.empty
    Engine(serverName, serverVersion, promoted ++ extra, instructions, 300000)

  private def copy[R1 <: R](
      serverName: String = serverName,
      serverVersion: String = serverVersion,
      promoted: Chunk[ToolCall[R1]] = promoted,
      catalogOps: Chunk[BoundOp[R1, ?, ?, ?]] = catalogOps,
      catalogOn: Boolean = catalogOn,
      instructions: Option[String] = instructions,
      path: String = path,
  ): Mcp[R1] =
    new Mcp(serverName, serverVersion, promoted, catalogOps, catalogOn, instructions, path)
end Mcp

object Mcp:
  val ProtocolVersion = JsonRpc.ProtocolVersion

  def from[R](apis: Api[R]*): Either[String, Mcp[R]] =
    val ops      = Chunk.fromIterable(apis).flatMap(_.ops)
    val promoted = ops.filter(o => o.endpoint.doc.promoted && OpArgs.promotable(o.endpoint.doc))
    val catalog  = ops.filter(o => OpArgs.promotable(o.endpoint.doc))
    val built    = promoted.map(Engine.bound)
    val failed   = built.collect { case Left(e) => e }
    val tools    = built.collect { case Right(t) => t }
    val names    = tools.map(_.name)
    val dupes    = names.diff(names.distinct)
    if failed.nonEmpty then Left(failed.mkString("; "))
    else if dupes.nonEmpty then Left(s"duplicate MCP tool names: ${dupes.distinct.mkString(", ")}")
    else
      val title   = apis.headOption.map(_.title).getOrElse("heddle")
      val version = apis.headOption.map(_.version).getOrElse("0.0.0")
      Right(new Mcp(title, version, tools, catalog, false, None, "mcp"))
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

  private def native[R, A, B](name: String, description: String, f: A => ZIO[R, String, B])(using
      sa: Schema[A],
      ja: JsonCodec[A],
      sb: Schema[B],
      jb: JsonCodec[B],
  ): ToolCall[R] =
    val defn = ToolDef(
      name = name,
      description = description,
      inputSchema = SchemaJson.render(sa.doc),
      outputSchema = Some(SchemaJson.render(sb.doc)),
      annotations = obj(),
    )
    new ToolCall[R]:
      def name: String                                                    = defn.name
      def definition: ToolDef                                             = defn
      def call(args: Json, headers: Headers): ZIO[R, Nothing, CallResult] =
        ja.decoder.decodeJson(args.toJson) match
          case Left(msg) => ZIO.succeed(CallResult.Failed(msg))
          case Right(a)  =>
            f(a).fold(
              CallResult.Failed(_),
              b =>
                val raw  = jb.encoder.encodeJson(b).toString
                val json = raw.fromJson[Json].getOrElse(Json.Str(raw))
                CallResult.Ok(json, raw),
            )
    end new
  end native

  private def catalogTools[R](ops: Chunk[BoundOp[R, ?, ?, ?]]): Chunk[ToolCall[R]] =
    val byName = ops.map(o => o.endpoint.doc.toolName -> o).toMap
    Chunk(searchTool(ops), invokeTool(byName))

  private def searchTool[R](ops: Chunk[BoundOp[R, ?, ?, ?]]): ToolCall[R] =
    val schema = obj(
      "type"       -> Json.Str("object"),
      "properties" -> obj("query" -> obj("type" -> Json.Str("string"))),
      "required"   -> Json.Arr(Json.Str("query")),
    )
    val defn = ToolDef("search_operations", "Search API operations by name, path, or summary", schema, None, obj())
    new ToolCall[R]:
      def name: String                                                    = defn.name
      def definition: ToolDef                                             = defn
      def call(args: Json, headers: Headers): ZIO[R, Nothing, CallResult] =
        val q = args match
          case o: Json.Obj =>
            o.get("query") match
              case Some(Json.Str(s)) => s.toLowerCase
              case _                 => ""
          case _ => ""
        val hits = ops.filter { op =>
          val d    = op.endpoint.doc
          val blob =
            s"${d.toolName} ${d.pathTemplate} ${d.summary.getOrElse("")} ${d.description.getOrElse("")}".toLowerCase
          q.isEmpty || blob.contains(q)
        }
        val arr = Json.Arr(hits.map { op =>
          val d = op.endpoint.doc
          obj(
            "name"    -> Json.Str(d.toolName),
            "method"  -> Json.Str(d.method.render),
            "path"    -> Json.Str(d.pathTemplate),
            "summary" -> Json.Str(d.summary.getOrElse("")),
          )
        })
        ZIO.succeed(CallResult.Ok(arr, arr.toJson))
      end call
    end new
  end searchTool

  private def invokeTool[R](byName: Map[String, BoundOp[R, ?, ?, ?]]): ToolCall[R] =
    val schema = obj(
      "type"       -> Json.Str("object"),
      "properties" -> obj(
        "operationId" -> obj("type" -> Json.Str("string")),
        "arguments"   -> obj("type" -> Json.Str("object")),
      ),
      "required" -> Json.Arr(Json.Str("operationId")),
    )
    val defn = ToolDef("invoke", "Invoke an API operation by name", schema, None, obj())
    new ToolCall[R]:
      def name: String                                                    = defn.name
      def definition: ToolDef                                             = defn
      def call(args: Json, headers: Headers): ZIO[R, Nothing, CallResult] =
        val (id, inner) = args match
          case o: Json.Obj =>
            val n = o
              .get("operationId")
              .flatMap:
                case Json.Str(s) => Some(s)
                case _           => None
            val a = o.get("arguments").getOrElse(obj())
            (n, a)
          case _ => (None, obj())
        id match
          case None    => ZIO.succeed(CallResult.Failed("missing operationId"))
          case Some(n) =>
            byName.get(n) match
              case None     => ZIO.succeed(CallResult.Failed(s"Unknown operation: $n"))
              case Some(op) => Engine.runBound(op, inner, headers)
      end call
    end new
  end invokeTool
end Mcp
