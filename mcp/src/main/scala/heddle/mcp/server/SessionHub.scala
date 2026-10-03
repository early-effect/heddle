package heddle.mcp.server

import heddle.mcp.protocol.{Message, RpcError}
import zio.*
import zio.stream.ZStream

/** Sessions for one server value. `Mcp` is built before any effect runs, so this table is a concurrent map rather than
  * a `Ref`. A session id is issued at `initialize` and is the only name `GET` and `resources/subscribe` accept.
  */
final class SessionHub:
  private val sessions = ConcurrentMap.empty[String, Session]

  def open(id: String): UIO[Unit] =
    ZIO.succeed {
      sessions.putIfAbsent(id, new Session)
      ()
    }

  def close(id: String): UIO[Unit] =
    ZIO.succeed(sessions.remove(id)).flatMap {
      case None    => ZIO.unit
      case Some(s) => s.shutdown
    }

  def contains(id: String): UIO[Boolean] =
    ZIO.succeed(sessions.contains(id))

  /** `false` when `id` is not an open session. */
  def subscribe(id: String, uri: String): UIO[Boolean] =
    ZIO.succeed {
      sessions.get(id) match
        case None    => false
        case Some(s) =>
          s.subscribe(uri)
          true
    }

  /** `false` when `id` is not an open session. An unknown URI is still a success. */
  def unsubscribe(id: String, uri: String): UIO[Boolean] =
    ZIO.succeed {
      sessions.get(id) match
        case None    => false
        case Some(s) =>
          s.unsubscribe(uri)
          true
    }

  def listeners(id: String): UIO[Int] =
    ZIO.succeed(sessions.get(id).fold(0)(_.listeners))

  /** `uri` set: only sessions that subscribed to it. `uri` empty: every open session. */
  def publish(note: Message.Notification, uri: Option[String]): UIO[Unit] =
    ZIO.foreachDiscard(sessions.values)(_.offer(note, uri))

  /** The queue a GET stream reads. Registered before the effect returns, for as long as the scope lives. */
  def attach(id: String): ZIO[Scope, RpcError, Queue[Message]] =
    ZIO.succeed(sessions.get(id)).flatMap {
      case None    => ZIO.fail(RpcError.InvalidRequest("unknown session"))
      case Some(s) => s.listen
    }

  /** Events for `id`, for as long as the stream is pulled. An unknown id fails before the first event. */
  def events(id: String): ZStream[Any, RpcError, Message] =
    ZStream.unwrapScoped(attach(id).map(ZStream.fromQueue(_)))
end SessionHub

private final class Session:
  private val uris = ConcurrentMap.empty[String, Unit]
  private val ears = ConcurrentMap.empty[Long, Queue[Message]]
  private val next = new java.util.concurrent.atomic.AtomicLong

  def subscribe(uri: String): Unit =
    uris.put(uri, ()); ()
  def unsubscribe(uri: String): Unit =
    uris.remove(uri); ()
  def listeners: Int = ears.size

  def listen: ZIO[Scope, Nothing, Queue[Message]] =
    for
      q <- Queue.unbounded[Message]
      n = next.incrementAndGet()
      _ <- ZIO.succeed(ears.put(n, q))
      _ <- ZIO.addFinalizer(ZIO.succeed(ears.remove(n)) *> q.shutdown)
    yield q

  def offer(note: Message.Notification, uri: Option[String]): UIO[Unit] =
    val wanted = uri.fold(true)(uris.contains)
    if !wanted then ZIO.unit
    else ZIO.foreachDiscard(ears.values)(_.offer(note).unit)

  def shutdown: UIO[Unit] =
    val queues = ears.values
    ears.clear()
    ZIO.foreachDiscard(queues)(_.shutdown)
end Session
