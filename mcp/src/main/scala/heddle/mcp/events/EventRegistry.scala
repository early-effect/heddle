package heddle.mcp.events

import heddle.http.header.Headers
import heddle.mcp.protocol.EventName
import zio.json.ast.Json
import zio.{Chunk, UIO, ZIO}

/** Declarations, limits, and the two hooks a server author supplies. The layer and the gateway are built from this, so
  * a later `withTool` has to carry it or the events disappear.
  */
private[heddle] final case class EventRegistry[-R](
    decls: Chunk[ErasedEvent[?, ?]],
    policy: TtlPolicy,
    read: Headers => ZIO[R, Nothing, Option[Principal]],
    allow: (Principal, EventName, Json.Obj) => UIO[Boolean],
)

private[heddle] object EventRegistry:
  val empty: EventRegistry[Any] =
    EventRegistry(
      Chunk.empty,
      TtlPolicy.default,
      _ => ZIO.none,
      (_, _, _) => ZIO.succeed(true),
    )
