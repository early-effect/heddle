package heddle.mcp.client

import heddle.LinePipe
import heddle.client.Client
import heddle.http.{Body, MediaType, Method, Request, Response, Status, Url}
import heddle.http.header.Headers
import heddle.mcp.protocol.*
import heddle.mcp.transport.Http as HttpTransport
import heddle.sse.{ServerSentEvent, SseCodec}
import zio.*
import zio.json.*
import zio.json.ast.Json

/** One request the carrier has sent and not yet finished. `notes` are filled before `answer` completes. */
private[client] final case class Inflight(
    answer: Promise[McpError, Message],
    notes: Ref[Chunk[Message.Notification]],
)

/** How messages reach one server. `submit` sends one request; the reader completes `flight.answer`. */
private[client] trait Carrier:
  def submit(req: Message.Request, era: Era, flight: Inflight): IO[McpError, Unit]
  def forget(id: RequestId): UIO[Unit]
  def notify(note: Message.Notification, era: Era): IO[McpError, Unit]

  /** The session hub, from `connect`, before the handshake. A pipe publishes into it. HTTP forks each read into
    * `scope`.
    */
  def attach(notes: Hub[Message.Notification], scope: Scope): UIO[Unit]

  /** Attach the session's server-to-client GET stream. A pipe already carries those lines, so it does nothing. */
  def openEvents(sink: Hub[Message.Notification]): ZIO[Scope, McpError, Unit]
end Carrier

/** Streamable HTTP. Each request is one POST; the answer is a JSON body or an event stream carrying it. */
private[client] final class HttpCarrier private (
    client: Client,
    url: Url,
    headers: Headers,
    session: Ref[Option[String]],
    bound: Ref[Option[HttpCarrier.Bound]],
    fibers: Ref[Map[RequestId, Fiber[McpError, Unit]]],
) extends Carrier:
  def submit(req: Message.Request, era: Era, flight: Inflight): IO[McpError, Unit] =
    bound.get.flatMap {
      case None =>
        val missing = McpError.Protocol("session is not attached")
        flight.answer.fail(missing).unit *> ZIO.fail(missing)
      case Some(b) =>
        val read =
          ZIO
            .scoped {
              post(req.json, req.method, req.params, era).flatMap { res =>
                remember(res) *> readAnswer(res, req.id, flight)
              }
            }
            .catchAll(e => flight.answer.fail(e).unit)
        read.forkIn(b.scope).flatMap(fiber => fibers.update(_ + (req.id -> fiber)))
    }

  override def forget(id: RequestId): UIO[Unit] =
    fibers.modify(m => (m.get(id), m - id)).flatMap {
      case Some(fiber) => fiber.interrupt.unit
      case None        => ZIO.unit
    }

  def notify(note: Message.Notification, era: Era): IO[McpError, Unit] =
    ZIO.scoped(post(note.json, note.method, note.params, era).flatMap(remember)).unit

  override def attach(notes: Hub[Message.Notification], scope: Scope): UIO[Unit] =
    bound.set(Some(HttpCarrier.Bound(notes, scope)))

  override def openEvents(sink: Hub[Message.Notification]): ZIO[Scope, McpError, Unit] =
    session.get.flatMap {
      case None     => ZIO.fail(McpError.Protocol("no session id"))
      case Some(id) =>
        val streamed = headers.add("Accept", MediaType.EventStream.render).add(HttpTransport.SessionHeader, id)
        client.streaming(Request(Method.GET, url, streamed)).mapError(McpError.Transport(_)).flatMap { res =>
          res.headers.contentType match
            case Some(mt) if mt.isEventStream && res.status == Status.Ok =>
              SseCodec
                .stream(res.body.toStream)
                .mapError(McpError.Pipe(_))
                .foreach { ev =>
                  ev.data.fromJson[Json].toOption.flatMap(Message.decode(_).toOption) match
                    case Some(note: Message.Notification) => sink.publish(note)
                    case _                                => ZIO.unit
                }
                .forkScoped
                .unit
            case _ => ZIO.fail(McpError.Http(res.status))
        }
    }

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

  /** Walk the body in order. A notification is recorded, then published. The matching result completes the call and the
    * rest of the stream is not read.
    */
  private def readAnswer(res: Response, id: RequestId, flight: Inflight): IO[McpError, Unit] =
    res.headers.contentType match
      case Some(mt) if mt.isEventStream => readEvents(res, id, flight)
      case Some(mt) if mt.isJson        => readJson(res, flight)
      case _                            => flight.answer.fail(McpError.Http(res.status)).unit

  private def readEvents(res: Response, id: RequestId, flight: Inflight): IO[McpError, Unit] =
    SseCodec
      .stream(res.body.toStream)
      .mapError(McpError.Pipe(_))
      .mapZIO(ev => onEvent(ev, id, flight))
      .collectSome
      .runHead
      .flatMap {
        case Some(message) => flight.answer.succeed(message).unit
        case None          =>
          flight.answer.fail(McpError.Protocol("the event stream ended without an answer")).unit
      }

  private def onEvent(ev: ServerSentEvent, id: RequestId, flight: Inflight): UIO[Option[Message]] =
    ev.data.fromJson[Json].toOption.flatMap(Message.decode(_).toOption) match
      case Some(note: Message.Notification)       => record(flight, note).as(None)
      case Some(m @ Message.Result(`id`, _))      => ZIO.some(m)
      case Some(m @ Message.Error(Some(`id`), _)) => ZIO.some(m)
      case _                                      => ZIO.none

  private def readJson(res: Response, flight: Inflight): IO[McpError, Unit] =
    res.body.utf8.mapError(McpError.Pipe(_)).flatMap { raw =>
      raw.fromJson[Json].left.map(McpError.Protocol(_)).flatMap { j =>
        Message.decode(j).left.map(e => McpError.Protocol(e.error.text))
      } match
        case Left(e)        => flight.answer.fail(e).unit
        case Right(message) => flight.answer.succeed(message).unit
    }

  private def record(flight: Inflight, note: Message.Notification): UIO[Unit] =
    flight.answer.isDone.flatMap { done =>
      ZIO.unlessDiscard(done)(flight.notes.update(_ :+ note))
    } *> publish(note)

  private def publish(note: Message.Notification): UIO[Unit] =
    bound.get.flatMap {
      case Some(b) => b.notes.publish(note).unit
      case None    => ZIO.unit
    }
end HttpCarrier

private[client] object HttpCarrier:
  final case class Bound(notes: Hub[Message.Notification], scope: Scope)

  def make(client: Client, url: Url, headers: Headers): UIO[HttpCarrier] =
    for
      session <- Ref.make(Option.empty[String])
      bound   <- Ref.make(Option.empty[Bound])
      fibers  <- Ref.make(Map.empty[RequestId, Fiber[McpError, Unit]])
    yield HttpCarrier(client, url, headers, session, bound, fibers)
end HttpCarrier

/** Newline-delimited JSON-RPC over a pipe. One reader fiber routes answers to the request that is waiting for them, so
  * any number of requests can be in flight; a server's `ping` is answered, other server requests are refused. A
  * notification is recorded on every in-flight call, published, and the loop continues.
  */
private[client] final class PipeCarrier private (
    pipe: LinePipe,
    pending: Ref[Option[Map[RequestId, Inflight]]],
    writes: Semaphore,
    sink: Ref[Option[Hub[Message.Notification]]],
) extends Carrier:
  def submit(req: Message.Request, era: Era, flight: Inflight): IO[McpError, Unit] =
    val register = pending.modify {
      case None    => (false, None)
      case Some(m) => (true, Some(m.updated(req.id, flight)))
    }
    ZIO.ifZIO(register)(
      write(req.json).tapError(e => forget(req.id) *> flight.answer.fail(e).unit),
      ZIO.fail(McpError.Closed),
    )

  override def forget(id: RequestId): UIO[Unit] =
    pending.update(_.map(_ - id))

  def notify(note: Message.Notification, era: Era): IO[McpError, Unit] =
    write(note.json)

  override def attach(notes: Hub[Message.Notification], _scope: Scope): UIO[Unit] =
    sink.set(Some(notes))

  override def openEvents(_sink: Hub[Message.Notification]): ZIO[Scope, McpError, Unit] =
    ZIO.unit

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
      case Some(Right(note: Message.Notification))           => record(note)
      case Some(Right(Message.Request(id, Methods.Ping, _))) =>
        write(Message.Result(id, Json.Obj()).json).ignore
      case Some(Right(Message.Request(id, method, _))) =>
        write(Message.Error(Some(id), RpcError.methodNotFound(method)).json).ignore
      case _ => ZIO.unit

  private def record(note: Message.Notification): UIO[Unit] =
    pending.get.flatMap { waiting =>
      ZIO.foreachDiscard(waiting.fold(Iterable.empty[Inflight])(_.values)) { flight =>
        flight.answer.isDone.flatMap { done =>
          ZIO.unlessDiscard(done)(flight.notes.update(_ :+ note))
        }
      }
    } *> publish(note)

  private def publish(note: Message.Notification): UIO[Unit] =
    sink.get.flatMap {
      case Some(hub) => hub.publish(note).unit
      case None      => ZIO.unit
    }

  private def deliver(id: RequestId, message: Message): UIO[Unit] =
    pending.get.flatMap(_.flatMap(_.get(id)).fold(ZIO.unit)(_.answer.succeed(message).unit))

  private def closeAll(e: McpError): UIO[Unit] =
    pending.getAndSet(None).flatMap(ps => ZIO.foreachDiscard(ps.toList.flatMap(_.values))(_.answer.fail(e)))

  private def write(msg: Json.Obj): IO[McpError, Unit] =
    writes.withPermit(pipe.writeLine(msg.toJson).mapError(McpError.Pipe(_)))
end PipeCarrier

private[client] object PipeCarrier:
  def scoped(pipe: LinePipe): URIO[Scope, PipeCarrier] =
    for
      pending <- Ref.make(Option(Map.empty[RequestId, Inflight]))
      writes  <- Semaphore.make(1)
      sink    <- Ref.make(Option.empty[Hub[Message.Notification]])
      carrier = PipeCarrier(pipe, pending, writes, sink)
      _ <- carrier.read.forkScoped
    yield carrier
end PipeCarrier
