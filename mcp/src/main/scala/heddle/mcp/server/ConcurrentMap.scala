package heddle.mcp.server

/** A map several fibers may touch. The JVM and Native use a concurrent table. Scala.js uses a plain map: a fiber does
  * not yield inside one mutation.
  */
private[server] trait ConcurrentMap[K, V]:
  def put(key: K, value: V): Unit
  def putIfAbsent(key: K, value: V): Unit
  def remove(key: K): Option[V]
  def get(key: K): Option[V]
  def contains(key: K): Boolean
  def values: List[V]
  def size: Int
  def clear(): Unit

private[server] object ConcurrentMap:
  def empty[K, V]: ConcurrentMap[K, V] = ConcurrentMapPlatform.empty
