package heddle.mcp

import heddle.LinePipe
import heddle.endpoint.{Api, BoundOp, OpArgs, Schema, SchemaJson}
import heddle.http.Response
import heddle.http.header.{AuthScheme, Authorization, Headers}
import heddle.http.header.Authorization.given
import heddle.mcp.auth.ProtectedResource
import heddle.mcp.events.{
  Delivery,
  EventGateway,
  EventRegistry,
  EventRuntime,
  Events,
  Principal,
  Replay,
  StoreError,
  SubscriptionStore,
  TtlPolicy,
  VerificationPolicy,
}
import heddle.mcp.protocol.{
  Era,
  EventName,
  ExtensionId,
  Implementation,
  Message,
  Notifications,
  Structured,
  Tool,
  ToolName,
}
import heddle.mcp.server.{Engine, Offer, OperationStream, SessionHub, ToolCall}
import heddle.mcp.transport.{Http, Stdio}
import heddle.route.Routes
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, NonEmptyChunk, UIO, ZIO, ZLayer, ZNothing}

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
    registry: EventRegistry[R],
):
  private val hub = new SessionHub

  /** Tells every session that subscribed to `uri`. A session that did not subscribe hears nothing. */
  def resourceUpdated(uri: String): UIO[Unit] =
    hub.publish(Message.Notification(Notifications.ResourceUpdated, Json.Obj("uri" -> Json.Str(uri))), Some(uri))

  /** Tells every open session that `tools/list` changed. */
  def toolsChanged: UIO[Unit] =
    hub.publish(Message.Notification(Notifications.ToolsListChanged, Json.Obj()), None)

  /** Tells every open session that `resources/list` changed. */
  def resourcesChanged: UIO[Unit] =
    hub.publish(Message.Notification(Notifications.ResourcesListChanged, Json.Obj()), None)

  /** How many GET streams are attached to `id`. Zero when the session is unknown. */
  def listeners(id: String): UIO[Int] = hub.listeners(id)
  def serverName: String              = server.name
  def serverVersion: String           = server.version

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
    else Right(new Mcp(server, offer.copy(tools = offer.tools :+ t), catalogOps, instructions, path, registry))

  /** Resources clients list and read. A taken URI, here or already on the server, is a build error. */
  def withResources[R1](served: ServedResource[R1]*): Either[NonEmptyChunk[McpBuildError], Mcp[R & R1]] =
    val uris  = offer.resources.map(_.resource.uri) ++ served.map(_.resource.uri)
    val dupes = uris.diff(uris.distinct).distinct.map(McpBuildError.DuplicateResource(_))
    NonEmptyChunk.fromChunk(dupes) match
      case Some(errors) => Left(errors)
      case None         =>
        Right(
          new Mcp(server, offer.copy(resources = offer.resources ++ served), catalogOps, instructions, path, registry)
        )

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
              new Mcp(server, offer.copy(tools = offer.tools :+ merged(t)), catalogOps, instructions, path, registry)
            }
      case i =>
        Right(
          new Mcp(
            server,
            offer.copy(tools = offer.tools.updated(i, merged(offer.tools(i)))),
            catalogOps,
            instructions,
            path,
            registry,
          )
        )

  /** Advertises an extension in `server/discover` and `initialize` capabilities, with its settings. */
  def withExtension(id: ExtensionId, settings: Json.Obj = Json.Obj()): Mcp[R] =
    new Mcp(
      server,
      offer.copy(extensions = offer.extensions.updated(id, settings)),
      catalogOps,
      instructions,
      path,
      registry,
    )

  def routes: Routes[R, ZNothing] =
    Http.routes(engine, hub, path)

  def discovery(
      resource: String,
      authorizationServers: List[String],
      scopes: List[String] = Nil,
  ): Routes[Any, Nothing] =
    ProtectedResource.routes(resource, authorizationServers, scopes)

  def stdio(): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, hub, LinePipe.standard)

  def stdio(pipe: LinePipe): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, hub, pipe)

  def stdio(in: InputStream, out: OutputStream): ZIO[R, Throwable, Unit] =
    Stdio.run(engine, hub, LinePipe.streams(in, out))

  /** Answers one 2026-07-28 JSON-RPC value, as `POST /mcp` would. */
  def handle(json: Json, headers: Headers = Headers.empty): ZIO[R, Nothing, Option[Json]] =
    engine.handle(json, headers, Era.Stateless, None).map(_.map(_.json))

  private def engine: Engine[R] =
    Engine(server, offer, instructions, 300000, hub)

  /** A typed event. `In` and `Data` are pinned, and the accepting function is the last parameter list. */
  inline def event[In, Data](inline name: String, description: String = ""): Mcp.EventBuilder[R, In, Data] =
    Mcp.EventBuilder(this, EventName(name), description)

  /** Runs at subscribe and before each webhook delivery. A refusal is `Forbidden`. Poll of an authenticated caller uses
    * it once. The function is a memory read: it runs while the event log is locked.
    */
  def authorize(allow: (Principal, EventName, Json.Obj) => UIO[Boolean]): Mcp[R] =
    new Mcp(server, offer, catalogOps, instructions, path, registry.copy(allow = allow))

  /** The server's TTL. `floor` raises a short `ttlMs`, an omitted `ttlMs` uses `default`, and `cap` bounds a longer
    * one. `ttlMs: null` is the cap, not forever. The other fields of `TtlPolicy` are the attempt and subscription
    * limits. `sanitize` clamps a policy that cannot be granted.
    */
  def eventPolicy(policy: TtlPolicy): Mcp[R] =
    new Mcp(server, offer, catalogOps, instructions, path, registry.copy(policy = TtlPolicy.sanitize(policy)))

  /** Challenge policy for the first subscribe to a `(principal, url)`. `Required` is the default. `Skip` commits that
    * subscribe with no challenge POST.
    */
  def verificationPolicy(policy: VerificationPolicy): Mcp[R] =
    new Mcp(server, offer, catalogOps, instructions, path, registry.copy(verification = policy))

  /** The ring and the subscriptions. Build the layer from the final server: it closes over the declarations and the
    * allow hook. It does not provide the clock. The app runtime's clock grants each TTL.
    *
    * The no-argument layer keeps the subscriptions in memory. Pass a [[heddle.mcp.events.SubscriptionStore]] to load a
    * snapshot when the layer opens and write it back as subscriptions change. `SubscriptionStore.file` is JVM-only.
    */
  def eventsLayer: ZLayer[Any, Nothing, Events] =
    EventRuntime.layer(registry.decls, registry.policy, registry.verification, registry.allow)

  def eventsLayer(store: ZLayer[Any, StoreError, SubscriptionStore]): ZLayer[Any, StoreError, Events] =
    store >>> EventRuntime.stored(registry.decls, registry.policy, registry.verification, registry.allow)

  private[mcp] def offered: Offer[R] = offer

  private[mcp] def registered: EventRegistry[R] = registry

  private[mcp] def rebuild[R1 <: R](nextOffer: Offer[R1], next: EventRegistry[R1]): Mcp[R1] =
    new Mcp(server, nextOffer, catalogOps, instructions, path, next)

  private[mcp] def addEvent[In, Data](
      name: EventName,
      description: String,
      modes: NonEmptyChunk[Delivery],
      replay: Replay,
      accept: (In, Data) => Boolean,
  )(using
      Schema[In],
      JsonCodec[In],
      Schema[Data],
      JsonCodec[Data],
  ): Either[NonEmptyChunk[McpBuildError], Mcp[R & Events.Env]] =
    if registry.decls.exists(_.name == name) then Left(NonEmptyChunk(McpBuildError.DuplicateEvent(name)))
    else
      heddle.mcp.events.ErasedEvent.build(name, description, modes, replay, accept) match
        case Left(error)  => Left(NonEmptyChunk(error))
        case Right(event) =>
          val gateway = EventGateway.live[R & Events.Env](registry.read)
          val next    = Offer[R & Events.Env](offer.tools, offer.resources, offer.extensions, offer.streams, gateway)
          Right(new Mcp(server, next, catalogOps, instructions, path, registry.copy(decls = registry.decls :+ event)))

  private def copy(instructions: Option[String] = instructions, path: String = path): Mcp[R] =
    new Mcp(server, offer, catalogOps, instructions, path, registry)
end Mcp

object Mcp:
  /** Every promoted operation becomes a tool, or every reason it cannot is returned. A stream operation is not a tool:
    * a promoted one, a non-JSON input, or a name already taken is a build error, and the rest are listed for
    * `subscriptions/listen`.
    */
  def from[R](apis: Api[R]*): Either[NonEmptyChunk[McpBuildError], Mcp[R]] =
    val all      = Chunk.fromIterable(apis)
    val ops      = all.flatMap(_.ops)
    val promoted = ops.filter(o => o.endpoint.doc.promoted && OpArgs.promotable(o.endpoint.doc))
    val catalog  = ops.filter(o => OpArgs.promotable(o.endpoint.doc))
    val built    = promoted.map(ToolCall.bound)
    val tools    = built.collect { case Right(t) => t }
    val streamed = all.flatMap(_.subscriptions).map(OperationStream.from)
    val streams  = streamed.collect { case Right(s) => s }
    val names    = tools.map(_.tool.name) ++ streams.map(_.name)
    val dupes    = names.diff(names.distinct).distinct.map(McpBuildError.DuplicateTool(_))
    val errors   = built.collect { case Left(e) => e } ++ streamed.collect { case Left(e) => e } ++ dupes
    NonEmptyChunk.fromChunk(errors) match
      case Some(found) => Left(found)
      case None        =>
        val title   = apis.headOption.map(_.title).getOrElse("heddle")
        val version = apis.headOption.map(_.version).getOrElse("0.0.0")
        Right(
          new Mcp(
            Implementation(title, version),
            Offer(tools, Chunk.empty, Map.empty, streams),
            catalog,
            None,
            "mcp",
            EventRegistry.empty,
          )
        )

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

  final class EventBuilder[-R, In, Data](mcp: Mcp[R], name: EventName, description: String):
    def pollAndWebhook(
        accept: (In, Data) => Boolean
    )(using
        Schema[In],
        JsonCodec[In],
        Schema[Data],
        JsonCodec[Data],
    ): Either[NonEmptyChunk[McpBuildError], Mcp[R & Events.Env]] =
      using(NonEmptyChunk(Delivery.Poll, Delivery.Webhook), Replay.default)(accept)

    def poll(
        accept: (In, Data) => Boolean
    )(using
        Schema[In],
        JsonCodec[In],
        Schema[Data],
        JsonCodec[Data],
    ): Either[NonEmptyChunk[McpBuildError], Mcp[R & Events.Env]] =
      using(NonEmptyChunk(Delivery.Poll), Replay.default)(accept)

    def webhook(replay: Replay)(
        accept: (In, Data) => Boolean
    )(using
        Schema[In],
        JsonCodec[In],
        Schema[Data],
        JsonCodec[Data],
    ): Either[NonEmptyChunk[McpBuildError], Mcp[R & Events.Env]] =
      using(NonEmptyChunk(Delivery.Webhook), replay)(accept)

    def using(modes: NonEmptyChunk[Delivery], replay: Replay)(
        accept: (In, Data) => Boolean
    )(using
        Schema[In],
        JsonCodec[In],
        Schema[Data],
        JsonCodec[Data],
    ): Either[NonEmptyChunk[McpBuildError], Mcp[R & Events.Env]] =
      mcp.addEvent(name, description, modes, replay, accept)
  end EventBuilder

  extension [R <: Events.Env](mcp: Mcp[R])

    /** Reads the caller of subscribe, poll, and unsubscribe. Call this after the event is declared. A missing or blank
      * principal is `Forbidden` on subscribe. Poll with no principal still reads the ring.
      */
    def principal[R1](read: Headers => ZIO[R1, Nothing, Option[Principal]]): Mcp[R & R1] =
      val reg     = mcp.registered
      val base    = mcp.offered
      val gateway = EventGateway.live[R & R1](read)
      val offered = Offer[R & R1](base.tools, base.resources, base.extensions, base.streams, gateway)
      mcp.rebuild(
        offered,
        EventRegistry[R & R1](reg.decls, reg.policy, reg.verification, read, reg.allow),
      )
  end extension
end Mcp
