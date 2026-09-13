package heddle.ws

import zio.Chunk

enum WebSocketFrame:
  case Text(text: String)
  case Binary(bytes: Chunk[Byte])
  case Ping(bytes: Chunk[Byte])
  case Pong(bytes: Chunk[Byte])
  case Close(code: Int, reason: String)

trait WebSocket:
  def receive: zio.stream.ZStream[Any, Throwable, WebSocketFrame]
  def send(frame: WebSocketFrame): zio.Task[Unit]
  def close(code: Int = 1000, reason: String = ""): zio.Task[Unit]
