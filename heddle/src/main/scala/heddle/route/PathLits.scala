package heddle.route

import java.nio.charset.StandardCharsets
import heddle.internal.Ascii
import zio.Chunk

/** Route literals registered as byte keys so request path slices reuse those strings. */
private[heddle] object PathLits:
  private val table = java.util.concurrent.ConcurrentHashMap[BytesView, String]()

  def register(value: String): Unit =
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    table.putIfAbsent(BytesView.copy(bytes), value)
    ()

  def intern(raw: Chunk[Byte], from: Int, until: Int): String =
    val hit = table.get(BytesView.view(raw, from, until))
    if hit ne null then hit else Ascii.string(raw, from, until)
end PathLits
