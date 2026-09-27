package heddle.internal

/** The one place a Java map's `null` for "absent" is read. Allocation-free, for the intern tables on parse paths. */
private[heddle] object JavaMaps:
  inline def getOr[K, V <: AnyRef](map: java.util.Map[K, V], key: K)(inline orElse: => V): V =
    val found = map.get(key)
    if found eq null then orElse else found
