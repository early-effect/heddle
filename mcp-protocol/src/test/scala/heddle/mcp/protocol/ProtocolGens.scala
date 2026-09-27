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
      .map(s => ToolName.from(s).toOption.get)

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
  end jsonAt

  def obj(depth: Int = 2): Gen[Any, Json.Obj] =
    Gen.mapOfBounded(0, 3)(Gen.alphaNumericStringBounded(1, 6), jsonAt(depth)).map(m => Json.Obj(Chunk.fromIterable(m)))

  val requestId: Gen[Any, RequestId] = Gen.oneOf(Gen.long.map(RequestId.Num(_)), text.map(RequestId.Str(_)))

  val version: Gen[Any, ProtocolVersion] =
    Gen.oneOf(Gen.const(ProtocolVersion.Current), Gen.const(ProtocolVersion.Legacy), text.map(ProtocolVersion(_)))

  val rpcError: Gen[Any, RpcError] = Gen.oneOf(
    text.map(RpcError.ParseError(_)),
    text.map(RpcError.InvalidRequest(_)),
    text.map(RpcError.MethodNotFound(_)),
    text.map(RpcError.InvalidParams(_)),
    text.map(RpcError.Internal(_)),
    text.map(RpcError.HeaderMismatch(_)),
    text.map(RpcError.ResourceNotFound(_)),
    (text <*> Gen.chunkOfBounded(0, 3)(version)).map(RpcError.UnsupportedVersion(_, _)),
    (Gen.int(-31999, 31999) <*> text <*> Gen.option(obj())).map(RpcError.Other(_, _, _)),
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
  )
end ProtocolGens
