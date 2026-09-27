package heddle.ws

import heddle.internal.engine.ConnBuf
import zio.*

private[heddle] type WsUpgrade = (ConnBuf, heddle.internal.duplex.Sink) => Task[Unit]
