package heddle.mcp.events

import scala.annotation.unused
import zio.ZLayer

private[events] object SubscriptionStorePlatform:
  def file(@unused path: String, @unused capacity: Int): ZLayer[Any, StoreError, SubscriptionStore] =
    ZLayer.fail(StoreError.Unavailable)
