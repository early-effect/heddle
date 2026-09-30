package heddle.mcp.apps.ui

import heddle.mcp.apps.AppGens
import heddle.mcp.protocol.{
  CallToolResult,
  ContentBlock,
  Implementation,
  Message,
  RequestId,
  ResourceContents,
  Tool,
  ToolName,
}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object UiProtocolLawsSpec extends ZIOSpecDefault:
  private val word  = Gen.stringBounded(1, 10)(Gen.alphaNumericChar)
  private val text  = Gen.string
  private val px    = Gen.double(0, 4000)
  private val small = Gen.chunkOfBounded(0, 3)(
    word <*> Gen.oneOf(word.map(Json.Str(_)), Gen.int.map(Json.Num(_)), Gen.boolean.map(Json.Bool(_)))
  )
  private val args = small.map(fs => Json.Obj(fs.map((k, v) => k -> (v: Json))))

  private val toolName                             = word.map(ToolName.from).collect { case Right(n) => n }
  private val impl                                 = (word <*> word <*> Gen.option(word)).map(Implementation(_, _, _))
  private val modes                                = Gen.chunkOfBounded(0, 3)(Gen.elements(DisplayMode.values*))
  private val textBlock: Gen[Any, ContentBlock]    = text.map(ContentBlock.Text(_))
  private val downloadable: Gen[Any, ContentBlock] = Gen.oneOf(
    (word <*> word).map((u, n) => ContentBlock.ResourceLink(s"https://example.test/$u", n)),
    (word <*> text).map((u, t) =>
      ContentBlock.Embedded(ResourceContents.Text(s"file:///$u", Some("text/plain"), t, None))
    ),
  )

  private val caps =
    (Gen.option(modes) <*> Gen.option(Gen.option(Gen.boolean).map(ListChanged(_)))).map(AppCapabilities(_, _))

  private val viewRequest: Gen[Any, ViewRequest] = Gen.oneOf(
    (impl <*> caps).map(ViewRequest.Initialize(_, _)),
    word.map(w => ViewRequest.OpenLink(s"https://example.test/$w")),
    Gen.chunkOfBounded(1, 3)(downloadable).map(ViewRequest.DownloadFile(_)),
    Gen.chunkOfBounded(1, 3)(textBlock).map(ViewRequest.SendMessage(_)),
    Gen.elements(DisplayMode.values*).map(ViewRequest.RequestDisplayMode(_)),
    (Gen.chunkOfBounded(0, 2)(textBlock) <*> Gen.option(args)).map(ViewRequest.UpdateModelContext(_, _)),
    (toolName <*> args).map(ViewRequest.CallTool(_, _)),
    word.map(w => ViewRequest.ReadResource(s"ui://early-effect/$w")),
    Gen.const(ViewRequest.Ping),
  )

  private val viewNotification: Gen[Any, ViewNotification] = Gen.oneOf(
    Gen.const(ViewNotification.Initialized),
    Gen.const(ViewNotification.RequestTeardown),
    (Gen.option(px) <*> Gen.option(px)).map(ViewNotification.SizeChanged(_, _)),
    (Gen.elements(LoggingLevel.values*) <*> Gen.option(word) <*> text).map((l, n, t) =>
      ViewNotification.Log(l, n, Json.Str(t))
    ),
  )

  private val extent: Gen[Any, Extent] =
    Gen.oneOf(px.map(Extent.Fixed(_)), px.map(Extent.AtMost(_)), Gen.const(Extent.Free))

  private val styles: Gen[Any, HostStyles] =
    (Gen.mapOfBounded(0, 4)(Gen.elements(HostVar.values*), word) <*> Gen.option(text)).map(HostStyles(_, _))

  private val tool: Gen[Any, Tool] =
    (toolName <*> Gen.option(word)).map((n, d) =>
      Tool(n, description = d, inputSchema = Json.Obj("type" -> Json.Str("object")))
    )

  /** Unknown keys only: a known key in `extra` would decode into its typed field. */
  private val extra: Gen[Any, Json.Obj] = small.map(fs => Json.Obj(fs.map((k, v) => s"x-$k" -> (v: Json))))

  private val context: Gen[Any, HostContext] =
    for
      info     <- Gen.option((Gen.option(Gen.long.map(RequestId.Num(_))) <*> tool).map(ToolInfo(_, _)))
      theme    <- Gen.option(Gen.elements(Theme.values*))
      style    <- Gen.option(styles)
      mode     <- Gen.option(Gen.elements(DisplayMode.values*))
      avail    <- Gen.option(modes)
      dims     <- Gen.option((extent <*> extent).map(ContainerDimensions(_, _)))
      locale   <- Gen.option(Gen.elements("en-US", "fr-FR"))
      zone     <- Gen.option(Gen.elements("America/New_York", "UTC"))
      agent    <- Gen.option(word)
      platform <- Gen.option(Gen.elements(Platform.values*))
      device   <- Gen.option((Gen.option(Gen.boolean) <*> Gen.option(Gen.boolean)).map(DeviceCapabilities(_, _)))
      insets   <- Gen.option((px <*> px <*> px <*> px).map(SafeAreaInsets(_, _, _, _)))
      more     <- extra
    yield HostContext(info, theme, style, mode, avail, dims, locale, zone, agent, platform, device, insets, more)

  private val hostNotification: Gen[Any, HostNotification] = Gen.oneOf(
    args.map(HostNotification.ToolInput(_)),
    args.map(HostNotification.ToolInputPartial(_)),
    (Gen.chunkOfBounded(0, 2)(textBlock) <*> Gen.option(args)).map((c, s) =>
      HostNotification.ToolResult(CallToolResult(c, s))
    ),
    Gen.option(text).map(HostNotification.ToolCancelled(_)),
    context.map(HostNotification.HostContextChanged(_)),
  )

  private val sandbox: Gen[Any, SandboxMessage] = Gen.oneOf(
    Gen.const(SandboxMessage.ProxyReady),
    Gen.const(SandboxMessage.Navigated),
    (text <*> Gen.option(Gen.const("allow-scripts")) <*> AppGens.network <*> AppGens.permissions).map((h, s, n, p) =>
      SandboxMessage.ResourceReady(h, s, SandboxGrant(n, p))
    ),
  )

  /** Through JSON text and back, as `postMessage` of a serialized message would carry it. */
  private def wire(m: Message): Either[String, Message] =
    m.json.toJson.fromJson[Json].flatMap(Message.decode(_).left.map(_.error.toString))

  def spec = suite("MCP Apps view protocol")(
    test("every view request survives the wire, whatever its id"):
      check(viewRequest, Gen.long) { (req, id) =>
        val back = wire(req.message(RequestId.Num(id))).flatMap {
          case Message.Request(RequestId.Num(`id`), m, p) => ViewRequest.decode(m, p).left.map(_.message)
          case other                                      => Left(s"not the request: $other")
        }
        assertTrue(back == Right(req))
      }
    ,
    test("every notification in either direction, and every sandbox message, survives the wire"):
      check(viewNotification, hostNotification, sandbox) { (v, h, s) =>
        def back[A](m: Message.Notification)(decode: (String, Json.Obj) => Either[UiError, A]) =
          wire(m).flatMap {
            case Message.Notification(method, p) => decode(method, p).left.map(_.message)
            case other                           => Left(s"not a notification: $other")
          }
        assertTrue(
          back(v.message)(ViewNotification.decode) == Right(v),
          back(h.message)(HostNotification.decode) == Right(h),
          back(s.message)(SandboxMessage.decode) == Right(s),
        )
      }
    ,
    test("a host context, unknown fields included, survives the wire"):
      check(context)(c => assertTrue(c.toJson.fromJson[HostContext] == Right(c)))
    ,
    test("a patch replaces only the fields it names: empty is the identity, and merging is associative"):
      check(context, context, context) { (a, b, c) =>
        assertTrue(
          a.merge(HostContext.empty) == a,
          HostContext.empty.merge(a) == a,
          a.merge(b).merge(c) == a.merge(b.merge(c)),
          a.merge(b).theme == b.theme.orElse(a.theme),
          a.merge(b).containerDimensions == b.containerDimensions.orElse(a.containerDimensions),
        )
      }
    ,
    test("the spec's example host context decodes to exactly what it says"):
      val raw =
        """{"theme":"dark","styles":{"variables":{"--color-background-primary":"light-dark(#ffffff, #171717)",
          |"--font-sans":"Anthropic Sans, sans-serif","--not-a-standard-var":"x"},"css":{"fonts":"@font-face {}"}},
          |"displayMode":"inline","containerDimensions":{"width":400,"maxHeight":600}}""".stripMargin
      assertTrue(
        raw.fromJson[HostContext] == Right(
          HostContext(
            theme = Some(Theme.Dark),
            styles = Some(
              HostStyles(
                Map(
                  HostVar.ColorBackgroundPrimary -> "light-dark(#ffffff, #171717)",
                  HostVar.FontSans               -> "Anthropic Sans, sans-serif",
                ),
                Some("@font-face {}"),
              )
            ),
            displayMode = Some(DisplayMode.Inline),
            containerDimensions = Some(ContainerDimensions(Extent.AtMost(600), Extent.Fixed(400))),
          )
        )
      )
    ,
    test("a message the protocol does not allow is refused with the reason"):
      val someone = Json.Obj("role" -> Json.Str("assistant"), "content" -> Json.Arr())
      val prose   = Json.Obj("contents" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("x"))))
      assertTrue(
        ViewRequest.decode("ui/eval", Json.Obj()) == Left(UiError.UnknownMethod("ui/eval")),
        ViewRequest.decode("ui/message", someone) == Left(
          UiError.BadParams("ui/message", "role must be user, not assistant")
        ),
        ViewRequest.decode("ui/download-file", prose).left.toOption.exists(_.message.contains("embedded resources")),
        ViewRequest.decode("tools/call", Json.Obj("name" -> Json.Str("no spaces"))).isLeft,
        HostNotification.decode("ui/notifications/tool-result", Json.Obj("content" -> Json.Str("x"))).isLeft,
      )
    ,
    test("only sandbox messages carry the reserved prefix, so a relay forwards everything else"):
      check(viewRequest, viewNotification, hostNotification, sandbox) { (r, v, h, s) =>
        assertTrue(
          s.method.startsWith(SandboxMessage.Prefix),
          !r.method.startsWith(SandboxMessage.Prefix),
          !v.method.startsWith(SandboxMessage.Prefix),
          !h.method.startsWith(SandboxMessage.Prefix),
        )
      },
  ) @@ TestAspect.timeout(60.seconds)
end UiProtocolLawsSpec
