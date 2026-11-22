package heddle.mcp.events

import heddle.http.header.Headers
import heddle.mcp.protocol.{ClientRequest, Methods, RpcError}
import zio.json.ast.Json
import zio.{UIO, URIO, ZIO}

/** What `Engine` calls for the events methods. `none` is a server that did not declare an event. The result object has
  * no `resultType`; the engine adds it.
  */
private[heddle] trait EventGateway[-R]:
  def enabled: Boolean
  def list(cursor: Option[String]): URIO[R, Either[RpcError, Json.Obj]]
  def poll(headers: Headers, request: ClientRequest.PollEvents): URIO[R, Either[RpcError, Json.Obj]]
  def subscribe(headers: Headers, request: ClientRequest.SubscribeEvents): URIO[R, Either[RpcError, Json.Obj]]
  def unsubscribe(headers: Headers, request: ClientRequest.UnsubscribeEvents): URIO[R, Either[RpcError, Json.Obj]]

private[heddle] object EventGateway:
  val none: EventGateway[Any] = new EventGateway[Any]:
    def enabled: Boolean                                              = false
    def list(cursor: Option[String]): UIO[Either[RpcError, Json.Obj]] =
      ZIO.left(RpcError.methodNotFound(Methods.ListEvents))
    def poll(headers: Headers, request: ClientRequest.PollEvents): UIO[Either[RpcError, Json.Obj]] =
      ZIO.left(RpcError.methodNotFound(Methods.PollEvents))
    def subscribe(headers: Headers, request: ClientRequest.SubscribeEvents): UIO[Either[RpcError, Json.Obj]] =
      ZIO.left(RpcError.methodNotFound(Methods.SubscribeEvents))
    def unsubscribe(headers: Headers, request: ClientRequest.UnsubscribeEvents): UIO[Either[RpcError, Json.Obj]] =
      ZIO.left(RpcError.methodNotFound(Methods.UnsubscribeEvents))

  /** `R` already has the sender. The bound is what lets the body ask for `Events`. */
  def live[R <: Events.Env](read: Headers => ZIO[R, Nothing, Option[Principal]]): EventGateway[R] =
    new EventGateway[R]:
      def enabled: Boolean                                                  = true
      def list(cursor: Option[String]): URIO[R, Either[RpcError, Json.Obj]] =
        ZIO.serviceWithZIO[Events](_.list(cursor))
      def poll(headers: Headers, request: ClientRequest.PollEvents): URIO[R, Either[RpcError, Json.Obj]] =
        principal(headers).flatMap { who =>
          ZIO.serviceWithZIO[Events](_.poll(who, request))
        }
      def subscribe(headers: Headers, request: ClientRequest.SubscribeEvents): URIO[R, Either[RpcError, Json.Obj]] =
        principal(headers).flatMap { who =>
          ZIO.serviceWithZIO[Events](_.subscribe(who, request))
        }
      def unsubscribe(headers: Headers, request: ClientRequest.UnsubscribeEvents): URIO[R, Either[RpcError, Json.Obj]] =
        principal(headers).flatMap { who =>
          ZIO.serviceWithZIO[Events](_.unsubscribe(who, request))
        }

      private def principal(headers: Headers): URIO[R, Option[Principal]] =
        read(headers).map(_.filter(_.value.nonEmpty))
end EventGateway
