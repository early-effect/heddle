package heddle.mcp.client

import heddle.LinePipe
import heddle.client.Client
import heddle.http.{Body, MediaType, Method, Request, Response, Url}
import heddle.http.header.Headers
import heddle.mcp.protocol.*
import heddle.mcp.transport.Http as HttpTransport
import heddle.sse.SseCodec
import zio.*
import zio.json.*
import zio.json.ast.Json

/** How messages reach one server. `exchange` returns the answer to that request's id. */
private[client] trait Carrier:
  def exchange(req: Message.Request, era: Era): IO[McpError, Message]
  def notify(note: Message.Notification, era: Era): IO[McpError, Unit]

/** Streamable HTTP. Each request is one POST; the answer is a JSON body or an event stream carrying it. */
private[client] final class HttpCarrier(client: Client, url: Url, headers: Headers, session: Ref[Option[String]])
    extends Carrier:
  def exchange(req: Message.Request, era: Era): IO[McpError, Message] =
    ZIO.scoped {
      post(req.json, req.method, req.params, era).flatMap { res =>
        remember(res) *> answer(res, req.id)
      }
    }

  def notify(note: Message.Notification, era: Era): IO[McpError, Unit] =
    ZIO.scoped(post(note.json, note.method, note.params, era).flatMap(remember)).unit

  private def post(body: Json.Obj, method: String, params: Json.Obj, era: Era): ZIO[Scope, McpError, Response] =
    session.get.flatMap { sid =>
      val base = headers
        .add("Accept", s"${MediaType.Json.render}, ${MediaType.EventStream.render}")
        .add(
          HttpTransport.ProtocolHeader,
          era match
            case Era.Stateless => ProtocolVersion.Current.value
            case Era.Session   => ProtocolVersion.Legacy.value,
        )
      val routed = era match
        case Era.Stateless =>
          val named = params.get("name").collect { case Json.Str(n) if method == Methods.CallTool => n }
          named.fold(base.add(HttpTransport.MethodHeader, method))(n =>
            base.add(HttpTransport.MethodHeader, method).add(HttpTransport.NameHeader, n)
          )
        case Era.Session => sid.fold(base)(base.add(HttpTransport.SessionHeader, _))
      client
        .streaming(Request(Method.POST, url, routed, Body.json(body.toJson)))
        .mapError(McpError.Transport(_))
    }

  private def remember(res: Response): UIO[Unit] =
    res.header(HttpTransport.SessionHeader).fold(ZIO.unit)(id => session.set(Some(id)))

  private def answer(res: Response, id: RequestId): IO[McpError, Message] =
    res.headers.contentType match
      case Some(mt) if mt.isEventStream =>
        SseCodec
          .stream(res.body.toStream)
          .mapError(McpError.Pipe(_))
          .map(ev => ev.data.fromJson[Json].toOption.flatMap(Message.decode(_).toOption))
          .collectSome
          .collect { case m @ (Message.Result(`id`, _) | Message.Error(Some(`id`), _)) => m }
          .runHead
          .someOrFail(McpError.Protocol("the event stream ended without an answer"))
      case Some(mt) if mt.isJson =>
        res.body.utf8.mapError(McpError.Pipe(_)).flatMap { raw =>
          raw
            .fromJson[Json]
            .left
            .map(McpError.Protocol(_))
            .flatMap(j => Message.decode(j).left.map(e => McpError.Protocol(e.error.text))) match
            case Left(e)  => ZIO.fail(e)
            case Right(m) => ZIO.succeed(m)
        }
      case _ => ZIO.fail(McpError.Http(res.status))
end HttpCarrier

/** Newline-delimited JSON-RPC over a pipe. One reader fiber routes answers to the request that is waiting for them, so
  * any number of requests can be in flight; a server's `ping` is answered, other server requests are refused.
  */
private[client] final class PipeCarrier private (
    pipe: LinePipe,
    pending: Ref[Option[Map[RequestId, Promise[McpError, Message]]]],
    writes: Semaphore,
) extends Carrier:
  def exchange(req: Message.Request, era: Era): IO[McpError, Message] =
    Promise.make[McpError, Message].flatMap { answer =>
      val register = pending.modify {
        case None    => (false, None)
        case Some(m) => (true, Some(m.updated(req.id, answer)))
      }
      val forget = pending.update(_.map(_ - req.id))
      ZIO.ifZIO(register)(
        (write(req.json) *> answer.await).ensuring(forget),
        ZIO.fail(McpError.Closed),
      )
    }

  def notify(note: Message.Notification, era: Era): IO[McpError, Unit] =
    write(note.json)

  private def write(msg: Json.Obj): IO[McpError, Unit] =
    writes.withPermit(pipe.writeLine(msg.toJson).mapError(McpError.Pipe(_)))

  /** Runs until the pipe ends, then fails every waiting request with `Closed`. */
  private def read: UIO[Unit] =
    pipe.readLine.either.flatMap {
      case Right(Some(line)) => route(line) *> read
      case Right(None)       => closeAll(McpError.Closed)
      case Left(e)           => closeAll(McpError.Pipe(e))
    }

  private def route(line: String): UIO[Unit] =
    line.fromJson[Json].toOption.map(Message.decode) match
      case Some(Right(m @ Message.Result(id, _)))            => deliver(id, m)
      case Some(Right(m @ Message.Error(Some(id), _)))       => deliver(id, m)
      case Some(Right(Message.Request(id, Methods.Ping, _))) =>
        write(Message.Result(id, Json.Obj()).json).ignore
      case Some(Right(Message.Request(id, method, _))) =>
        write(Message.Error(Some(id), RpcError.methodNotFound(method)).json).ignore
      case _ => ZIO.unit

  private def deliver(id: RequestId, m: Message): UIO[Unit] =
    pending.get.flatMap(_.flatMap(_.get(id)).fold(ZIO.unit)(_.succeed(m).unit))

  private def closeAll(e: McpError): UIO[Unit] =
    pending.getAndSet(None).flatMap(ps => ZIO.foreachDiscard(ps.toList.flatMap(_.values))(_.fail(e)))
end PipeCarrier

private[client] object PipeCarrier:
  def scoped(pipe: LinePipe): URIO[Scope, PipeCarrier] =
    for
      pending <- Ref.make(Option(Map.empty[RequestId, Promise[McpError, Message]]))
      writes  <- Semaphore.make(1)
      carrier = PipeCarrier(pipe, pending, writes)
      _ <- carrier.read.forkScoped
    yield carrier
end PipeCarrier
