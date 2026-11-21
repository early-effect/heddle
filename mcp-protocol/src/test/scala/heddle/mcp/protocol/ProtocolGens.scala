package heddle.mcp.protocol

import zio.Chunk
import zio.json.ast.Json
import zio.test.Gen

/** Generators for wire values. `json` covers every JSON shape, so decoders meet input they were not built for. */
object ProtocolGens:
  val text: Gen[Any, String] =
    Gen.oneOf(Gen.alphaNumericStringBounded(0, 12), Gen.stringBounded(0, 12)(Gen.unicodeChar))

  val toolName: Gen[Any, ToolName] =
    Gen
      .stringBounded(1, 40)(Gen.oneOf(Gen.alphaNumericChar, Gen.elements('_', '-', '.')))
      .map(ToolName.from)
      .collect { case Right(name) => name }

  val eventName: Gen[Any, EventName] =
    Gen
      .stringBounded(1, 40)(Gen.oneOf(Gen.alphaNumericChar, Gen.elements('_', '-', '.')))
      .map(EventName.from)
      .collect { case Right(name) => name }

  val json: Gen[Any, Json] = Gen.suspend(jsonAt(3))

  private def jsonAt(depth: Int): Gen[Any, Json] =
    val leaf = Gen.oneOf(
      Gen.const(Json.Null),
      Gen.boolean.map(Json.Bool(_)),
      Gen.long.map(Json.Num(_)),
      Gen.double.filter(d => !d.isNaN && !d.isInfinite).map(Json.Num(_)),
      text.map(Json.Str(_)),
    )
    if depth <= 0 then leaf
    else
      Gen.oneOf(
        leaf,
        Gen.listOfBounded(0, 3)(jsonAt(depth - 1)).map(xs => Json.Arr(Chunk.fromIterable(xs))),
        obj(depth - 1),
      )

  def obj(depth: Int = 2): Gen[Any, Json.Obj] =
    Gen.mapOfBounded(0, 3)(Gen.alphaNumericStringBounded(1, 6), jsonAt(depth)).map(m => Json.Obj(Chunk.fromIterable(m)))

  val requestId: Gen[Any, RequestId] = Gen.oneOf(Gen.long.map(RequestId.Num(_)), text.map(RequestId.Str(_)))

  val version: Gen[Any, ProtocolVersion] =
    Gen.oneOf(Gen.const(ProtocolVersion.Current), Gen.const(ProtocolVersion.Legacy), text.map(ProtocolVersion(_)))

  private val reservedCodes = Set(
    -32700, -32600, -32601, -32602, -32603, -32002, -32020, -32022, -32023, -32024, -32025, -32026, -32027,
  )

  private val callbackFailure: Gen[Any, CallbackFailure] = Gen.elements(
    CallbackFailure.ConnectionRefused,
    CallbackFailure.Timeout,
    CallbackFailure.TlsError,
    CallbackFailure.Http4xx,
    CallbackFailure.Http5xx,
    CallbackFailure.ChallengeFailed,
  )

  private val ttlRequest: Gen[Any, TtlRequest] = Gen.oneOf(
    Gen.const(TtlRequest.Absent),
    Gen.const(TtlRequest.Unbounded),
    Gen.long(0, 100_000).map(TtlRequest.Millis(_)),
  )

  val rpcError: Gen[Any, RpcError] = Gen.oneOf(
    text.map(RpcError.ParseError(_)),
    text.map(RpcError.InvalidRequest(_)),
    text.map(RpcError.MethodNotFound(_)),
    text.map(RpcError.InvalidParams(_)),
    text.map(RpcError.Internal(_)),
    text.map(RpcError.HeaderMismatch(_)),
    text.map(RpcError.ResourceNotFound(_)),
    (text <*> Gen.chunkOfBounded(0, 3)(version)).map(RpcError.UnsupportedVersion(_, _)),
    Gen.option(obj()).map(RpcError.OperationFailed(_)),
    Gen.option(Gen.elements("event", "subscription")).map(RpcError.NotFound(_)),
    Gen.option(text).map(RpcError.Forbidden(_)),
    (text <*> Gen.option(Gen.int(0, 32))).map(RpcError.ResourceExhausted(_, _)),
    obj().map(RpcError.Unsupported(_)),
    callbackFailure.map(RpcError.CallbackEndpointError(_)),
    (Gen.int(-31999, 31999).filterNot(reservedCodes.contains) <*> text <*> Gen.option(obj()))
      .map(RpcError.Other(_, _, _)),
  )

  val message: Gen[Any, Message] = Gen.oneOf(
    (requestId <*> text <*> obj()).map(Message.Request(_, _, _)),
    (text <*> obj()).map(Message.Notification(_, _)),
    (requestId <*> obj()).map(Message.Result(_, _)),
    (Gen.option(requestId) <*> rpcError).map(Message.Error(_, _)),
  )

  private val meta: Gen[Any, Option[Json.Obj]] = Gen.option(obj(1))

  val annotations: Gen[Any, ToolAnnotations] =
    (Gen.option(text) <*> Gen.option(Gen.boolean) <*> Gen.option(Gen.boolean) <*> Gen.option(Gen.boolean) <*>
      Gen.option(Gen.boolean)).map(ToolAnnotations(_, _, _, _, _))

  val tool: Gen[Any, Tool] =
    (toolName <*> Gen.option(text) <*> Gen.option(text) <*> obj() <*> Gen.option(obj()) <*>
      Gen.option(annotations) <*> meta).map(Tool(_, _, _, _, _, _, _))

  val resourceContents: Gen[Any, ResourceContents] = Gen.oneOf(
    (text <*> Gen.option(text) <*> text <*> meta).map(ResourceContents.Text(_, _, _, _)),
    (text <*> Gen.option(text) <*> text <*> meta).map(ResourceContents.Blob(_, _, _, _)),
  )

  val content: Gen[Any, ContentBlock] = Gen.oneOf(
    (text <*> meta).map(ContentBlock.Text(_, _)),
    (text <*> text <*> meta).map(ContentBlock.Image(_, _, _)),
    (text <*> text <*> meta).map(ContentBlock.Audio(_, _, _)),
    (text <*> text <*> Gen.option(text) <*> Gen.option(text) <*> meta).map(ContentBlock.ResourceLink(_, _, _, _, _)),
    (resourceContents <*> meta).map(ContentBlock.Embedded(_, _)),
  )

  val callToolResult: Gen[Any, CallToolResult] =
    (Gen.chunkOfBounded(0, 3)(content) <*> Gen.option(obj()) <*> Gen.option(Gen.boolean) <*>
      meta).map(CallToolResult(_, _, _, _))

  val resource: Gen[Any, Resource] =
    (text <*> text <*> Gen.option(text) <*> Gen.option(text) <*> Gen.option(text) <*> meta)
      .map(Resource(_, _, _, _, _, _))

  val clientRequest: Gen[Any, ClientRequest] = Gen.oneOf(
    Gen.const(ClientRequest.Ping),
    Gen.const(ClientRequest.Discover),
    (version <*> obj() <*> Gen.option((text <*> text <*> Gen.option(text)).map(Implementation(_, _, _))))
      .map(ClientRequest.Initialize(_, _, _)),
    Gen.option(text).map(ClientRequest.ListTools(_)),
    (toolName <*> obj()).map(ClientRequest.CallTool(_, _)),
    Gen.option(text).map(ClientRequest.ListResources(_)),
    Gen.option(text).map(ClientRequest.ListResourceTemplates(_)),
    text.map(ClientRequest.ReadResource(_)),
    text.map(ClientRequest.SubscribeResource(_)),
    text.map(ClientRequest.UnsubscribeResource(_)),
    (toolName <*> obj()).map(ClientRequest.Listen(_, _)),
    Gen.option(text).map(ClientRequest.ListEvents(_)),
    (eventName <*> obj() <*> Gen.option(text) <*> Gen.option(Gen.long(0, 100_000)) <*> Gen.option(Gen.int(0, 50)))
      .map(ClientRequest.PollEvents(_, _, _, _, _)),
    (eventName <*> obj() <*> text <*> text <*> Gen.option(text) <*> Gen.option(Gen.long(0, 100_000)) <*> ttlRequest)
      .map(ClientRequest.SubscribeEvents(_, _, _, _, _, _, _)),
    (eventName <*> obj() <*> text).map(ClientRequest.UnsubscribeEvents(_, _, _)),
    (eventName <*> obj() <*> Gen.option(text) <*> Gen.option(Gen.long(0, 100_000)) <*> Gen.option(Gen.long(0, 100_000)))
      .map(ClientRequest.StreamEvents(_, _, _, _, _)),
  )
end ProtocolGens
