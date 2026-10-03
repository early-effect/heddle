package heddle.mcp.server

import scala.collection.mutable

private[server] object ConcurrentMapPlatform:
  def empty[K, V]: ConcurrentMap[K, V] = new ConcurrentMap[K, V]:
    private val underlying          = mutable.HashMap.empty[K, V]
    def put(key: K, value: V): Unit =
      underlying.put(key, value); ()
    def putIfAbsent(key: K, value: V): Unit =
      if !underlying.contains(key) then underlying.update(key, value)
    def remove(key: K): Option[V] = underlying.remove(key)
    def get(key: K): Option[V]    = underlying.get(key)
    def contains(key: K): Boolean = underlying.contains(key)
    def values: List[V]           = underlying.values.toList
    def size: Int                 = underlying.size
    def clear(): Unit             = underlying.clear()
end ConcurrentMapPlatform
