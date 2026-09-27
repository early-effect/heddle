package heddle.mcp.apps.ui

import heddle.error.HeddleError
import heddle.mcp.protocol.{CallToolResult, ContentBlock, Implementation, Message, RequestId, ToolName}
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** Why a `postMessage` payload is not an MCP Apps message this revision understands. */
enum UiError(val message: String) extends HeddleError:
  case UnknownMethod(method: String)             extends UiError(s"no MCP Apps message is called $method")
  case BadParams(method: String, detail: String) extends UiError(s"$method: $detail")

/** MCP's logging levels (RFC 5424 severities), for `notifications/message`. */
enum LoggingLevel(val wire: String):
  case Debug     extends LoggingLevel("debug")
  case Info      extends LoggingLevel("info")
  case Notice    extends LoggingLevel("notice")
  case Warning   extends LoggingLevel("warning")
  case Error     extends LoggingLevel("error")
  case Critical  extends LoggingLevel("critical")
  case Alert     extends LoggingLevel("alert")
  case Emergency extends LoggingLevel("emergency")

object LoggingLevel:
  given JsonCodec[LoggingLevel] = UiProtocol.wire(values, _.wire, "log level")

/** The requests a view sends its host. `tools/call`, `resources/read`, and `ping` are MCP's own. */
enum ViewRequest(val method: String):
  case Initialize(appInfo: Implementation, capabilities: AppCapabilities, protocolVersion: String = UiProtocol.Version)
      extends ViewRequest("ui/initialize")
  case OpenLink(url: String)                       extends ViewRequest("ui/open-link")
  case DownloadFile(contents: Chunk[ContentBlock]) extends ViewRequest("ui/download-file")
  case SendMessage(content: Chunk[ContentBlock])   extends ViewRequest("ui/message")
  case RequestDisplayMode(mode: DisplayMode)       extends ViewRequest("ui/request-display-mode")
  case UpdateModelContext(content: Chunk[ContentBlock], structured: Option[Json.Obj])
      extends ViewRequest("ui/update-model-context")
  case CallTool(name: ToolName, arguments: Json.Obj) extends ViewRequest("tools/call")
  case ReadResource(uri: String)                     extends ViewRequest("resources/read")
  case Ping                                          extends ViewRequest("ping")

  def params: Json.Obj =
    this match
      case Initialize(info, caps, version) =>
        Json.Obj(
          "appInfo"         -> UiWire.json(info),
          "appCapabilities" -> UiWire.json(caps),
          "protocolVersion" -> Json.Str(version),
        )
      case OpenLink(url)            => Json.Obj("url" -> Json.Str(url))
      case DownloadFile(contents)   => Json.Obj("contents" -> UiWire.json(contents))
      case SendMessage(content)     => Json.Obj("role" -> Json.Str("user"), "content" -> UiWire.json(content))
      case RequestDisplayMode(mode) => Json.Obj("mode" -> UiWire.json(mode))
      case UpdateModelContext(c, s) =>
        Json.Obj(
          Chunk.fromIterable(Option.when(c.nonEmpty)("content" -> UiWire.json(c))) ++
            Chunk.fromIterable(s.map("structuredContent" -> _))
        )
      case CallTool(name, arguments) => Json.Obj("name" -> Json.Str(name.value), "arguments" -> arguments)
      case ReadResource(uri)         => Json.Obj("uri" -> Json.Str(uri))
      case Ping                      => Json.Obj()

  def message(id: RequestId): Message.Request = Message.Request(id, method, params)
end ViewRequest

object ViewRequest:
  def decode(method: String, params: Json.Obj): Either[UiError, ViewRequest] =
    val p = UiWire.Params(method, params)
    method match
      case "ui/initialize" =>
        for
          info    <- p.required[Implementation]("appInfo")
          caps    <- p.required[AppCapabilities]("appCapabilities")
          version <- p.required[String]("protocolVersion")
        yield Initialize(info, caps, version)
      case "ui/open-link"     => p.required[String]("url").map(OpenLink(_))
      case "ui/download-file" =>
        p.required[Chunk[ContentBlock]]("contents").flatMap { contents =>
          contents.collectFirst { case b if !downloadable(b) => b } match
            case Some(_) => Left(p.bad("contents may hold only embedded resources and resource links"))
            case None    => Right(DownloadFile(contents))
        }
      case "ui/message" =>
        p.required[String]("role").flatMap { role =>
          if role != "user" then Left(p.bad(s"role must be user, not $role"))
          else p.required[Chunk[ContentBlock]]("content").map(SendMessage(_))
        }
      case "ui/request-display-mode" => p.required[DisplayMode]("mode").map(RequestDisplayMode(_))
      case "ui/update-model-context" =>
        for
          content    <- p.optional[Chunk[ContentBlock]]("content")
          structured <- p.optional[Json.Obj]("structuredContent")
        yield UpdateModelContext(content.getOrElse(Chunk.empty), structured)
      case "tools/call" =>
        for
          raw       <- p.required[String]("name")
          name      <- ToolName.from(raw).left.map(e => p.bad(e.message))
          arguments <- p.optional[Json.Obj]("arguments")
        yield CallTool(name, arguments.getOrElse(Json.Obj()))
      case "resources/read" => p.required[String]("uri").map(ReadResource(_))
      case "ping"           => Right(Ping)
      case other            => Left(UiError.UnknownMethod(other))
    end match
  end decode

  private def downloadable(b: ContentBlock): Boolean =
    b match
      case _: ContentBlock.Embedded | _: ContentBlock.ResourceLink => true
      case _                                                       => false
end ViewRequest

/** The notifications a view sends its host. */
enum ViewNotification(val method: String):
  case Initialized extends ViewNotification("ui/notifications/initialized")
  case SizeChanged(width: Option[Double], height: Option[Double])
      extends ViewNotification("ui/notifications/size-changed")
  case RequestTeardown extends ViewNotification("ui/notifications/request-teardown")
  case Log(level: LoggingLevel, logger: Option[String], data: Json) extends ViewNotification("notifications/message")

  def params: Json.Obj =
    this match
      case Initialized | RequestTeardown => Json.Obj()
      case SizeChanged(w, h)             =>
        Json.Obj(
          Chunk.fromIterable(w.map("width" -> Json.Num(_))) ++ Chunk.fromIterable(h.map("height" -> Json.Num(_)))
        )
      case Log(level, logger, data) =>
        Json.Obj(
          Chunk("level" -> UiWire.json(level)) ++ Chunk.fromIterable(
            logger.map("logger" -> Json.Str(_))
          ) :+ ("data" -> data)
        )

  def message: Message.Notification = Message.Notification(method, params)
end ViewNotification

object ViewNotification:
  def decode(method: String, params: Json.Obj): Either[UiError, ViewNotification] =
    val p = UiWire.Params(method, params)
    method match
      case "ui/notifications/initialized"      => Right(Initialized)
      case "ui/notifications/request-teardown" => Right(RequestTeardown)
      case "ui/notifications/size-changed"     =>
        for
          width  <- p.optional[Double]("width")
          height <- p.optional[Double]("height")
        yield SizeChanged(width, height)
      case "notifications/message" =>
        for
          level  <- p.required[LoggingLevel]("level")
          logger <- p.optional[String]("logger")
          data   <- p.required[Json]("data")
        yield Log(level, logger, data)
      case other => Left(UiError.UnknownMethod(other))
    end match
  end decode
end ViewNotification

/** The notifications a host sends its view, after the view's `ui/notifications/initialized`. */
enum HostNotification(val method: String):
  case ToolInput(arguments: Json.Obj)         extends HostNotification("ui/notifications/tool-input")
  case ToolInputPartial(arguments: Json.Obj)  extends HostNotification("ui/notifications/tool-input-partial")
  case ToolResult(result: CallToolResult)     extends HostNotification("ui/notifications/tool-result")
  case ToolCancelled(reason: Option[String])  extends HostNotification("ui/notifications/tool-cancelled")
  case HostContextChanged(patch: HostContext) extends HostNotification("ui/notifications/host-context-changed")

  def params: Json.Obj =
    this match
      case ToolInput(arguments)        => Json.Obj("arguments" -> arguments)
      case ToolInputPartial(arguments) => Json.Obj("arguments" -> arguments)
      case ToolResult(result)          => UiWire.obj(result)
      case ToolCancelled(reason)       => Json.Obj(Chunk.fromIterable(reason.map("reason" -> Json.Str(_))))
      case HostContextChanged(patch)   => UiWire.obj(patch)

  def message: Message.Notification = Message.Notification(method, params)
end HostNotification

object HostNotification:
  def decode(method: String, params: Json.Obj): Either[UiError, HostNotification] =
    val p = UiWire.Params(method, params)
    method match
      case "ui/notifications/tool-input" =>
        p.optional[Json.Obj]("arguments").map(a => ToolInput(a.getOrElse(Json.Obj())))
      case "ui/notifications/tool-input-partial" =>
        p.optional[Json.Obj]("arguments").map(a => ToolInputPartial(a.getOrElse(Json.Obj())))
      case "ui/notifications/tool-result"          => p.whole[CallToolResult].map(ToolResult(_))
      case "ui/notifications/tool-cancelled"       => p.optional[String]("reason").map(ToolCancelled(_))
      case "ui/notifications/host-context-changed" => p.whole[HostContext].map(HostContextChanged(_))
      case other                                   => Left(UiError.UnknownMethod(other))
  end decode
end HostNotification

/** The one request a host sends its view: tear down, answered once the view has saved what it must. */
enum HostRequest(val method: String):
  case ResourceTeardown(reason: Option[String]) extends HostRequest("ui/resource-teardown")

  def params: Json.Obj =
    this match
      case ResourceTeardown(reason) => Json.Obj(Chunk.fromIterable(reason.map("reason" -> Json.Str(_))))

  def message(id: RequestId): Message.Request = Message.Request(id, method, params)

object HostRequest:
  def decode(method: String, params: Json.Obj): Either[UiError, HostRequest] =
    method match
      case "ui/resource-teardown" => UiWire.Params(method, params).optional[String]("reason").map(ResourceTeardown(_))
      case other                  => Left(UiError.UnknownMethod(other))

/** Between a web host and its sandbox relay only; the relay never forwards these to the view. */
enum SandboxMessage(val method: String):
  case ProxyReady extends SandboxMessage("ui/notifications/sandbox-proxy-ready")

  /** The view's HTML, with the CSP and permissions the host granted for it. */
  case ResourceReady(html: String, sandbox: Option[String], grant: SandboxGrant)
      extends SandboxMessage("ui/notifications/sandbox-resource-ready")

  def params: Json.Obj =
    this match
      case ProxyReady                          => Json.Obj()
      case ResourceReady(html, sandbox, grant) =>
        Json.Obj(
          Chunk("html" -> Json.Str(html)) ++ Chunk
            .fromIterable(sandbox.map("sandbox" -> Json.Str(_))) ++ UiWire.obj(grant).fields
        )

  def message: Message.Notification = Message.Notification(method, params)
end SandboxMessage

object SandboxMessage:
  /** Reserved by the spec: the relay handles every method with this prefix and forwards none of them. */
  val Prefix = "ui/notifications/sandbox-"

  def decode(method: String, params: Json.Obj): Either[UiError, SandboxMessage] =
    val p = UiWire.Params(method, params)
    method match
      case "ui/notifications/sandbox-proxy-ready"    => Right(ProxyReady)
      case "ui/notifications/sandbox-resource-ready" =>
        for
          html    <- p.required[String]("html")
          sandbox <- p.optional[String]("sandbox")
          grant   <- p.whole[SandboxGrant]
        yield ResourceReady(html, sandbox, grant)
      case other => Left(UiError.UnknownMethod(other))
  end decode
end SandboxMessage

/** JSON plumbing shared by the message codecs. */
private[ui] object UiWire:
  def json[A: JsonEncoder](a: A): Json = a.toJsonAST.fold(_ => Json.Null, identity)

  def obj[A: JsonEncoder](a: A): Json.Obj =
    json(a) match
      case o: Json.Obj => o
      case _           => Json.Obj()

  /** One message's params, read field by field; every failure names the method. */
  final case class Params(method: String, params: Json.Obj):
    def bad(detail: String): UiError = UiError.BadParams(method, detail)

    def required[A: JsonDecoder](name: String): Either[UiError, A] =
      params.get(name) match
        case None    => Left(bad(s"no $name"))
        case Some(j) => j.as[A].left.map(e => bad(s"$name: $e"))

    def optional[A: JsonDecoder](name: String): Either[UiError, Option[A]] =
      params.get(name) match
        case None | Some(Json.Null) => Right(None)
        case Some(j)                => j.as[A].map(Some(_)).left.map(e => bad(s"$name: $e"))

    def whole[A: JsonDecoder]: Either[UiError, A] = params.as[A].left.map(bad)
  end Params
end UiWire
