package heddle.mcp.events

import heddle.endpoint.{Schema, SchemaJson}
import heddle.mcp.McpBuildError
import heddle.mcp.protocol.{EventName, RpcError}
import zio.json.*
import zio.json.ast.Json
import zio.NonEmptyChunk

/** One declared event type, with the codecs erased behind `matches`. */
private[heddle] final class ErasedEvent[In, Data](
    val name: EventName,
    val description: String,
    val inputSchema: Json.Obj,
    val payloadSchema: Json.Obj,
    val modes: NonEmptyChunk[Delivery],
    val replay: Replay,
    decodeIn: JsonDecoder[In],
    decodeData: JsonDecoder[Data],
    accept: (In, Data) => Boolean,
):
  def supports(mode: Delivery): Boolean = modes.contains(mode)

  def arguments(json: Json.Obj): Either[RpcError, Unit] =
    decodeIn.fromJsonAST(json) match
      case Right(_) => Right(())
      case Left(_)  => Left(RpcError.InvalidParams("arguments do not match the event input"))

  def payload(json: Json): Boolean =
    decodeData.fromJsonAST(json).isRight

  def matches(args: Json.Obj, data: Json): Boolean =
    (decodeIn.fromJsonAST(args), decodeData.fromJsonAST(data)) match
      case (Right(in), Right(value)) => accept(in, value)
      case _                         => false
end ErasedEvent

private[heddle] object ErasedEvent:
  def build[In, Data](
      name: EventName,
      description: String,
      modes: NonEmptyChunk[Delivery],
      replay: Replay,
      accept: (In, Data) => Boolean,
  )(using
      inSchema: Schema[In],
      inCodec: JsonCodec[In],
      dataSchema: Schema[Data],
      dataCodec: JsonCodec[Data],
  ): Either[McpBuildError, ErasedEvent[In, Data]] =
    if !Replay.accepted(replay) then Left(McpBuildError.BadReplay)
    else
      for
        input   <- objSchema(name, inSchema)
        payload <- objSchema(name, dataSchema)
      yield
        val distinct = NonEmptyChunk.fromChunk(modes.toChunk.distinct).getOrElse(modes)
        new ErasedEvent(
          name,
          description,
          input,
          payload,
          distinct,
          replay,
          inCodec.decoder,
          dataCodec.decoder,
          accept,
        )

  private def objSchema[A](name: EventName, schema: Schema[A]): Either[McpBuildError, Json.Obj] =
    SchemaJson.render(schema.doc) match
      case obj: Json.Obj => Right(obj)
      case _             => Left(McpBuildError.EventSchema(name))
end ErasedEvent
