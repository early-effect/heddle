package heddle.ws

import heddle.internal.engine.ConnBuf
import zio.*

private[heddle] type WsUpgrade = (ConnBuf, Chunk[Byte] => Task[Unit]) => Task[Unit]
