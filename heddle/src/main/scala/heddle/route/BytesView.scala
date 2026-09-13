package heddle.route

import zio.Chunk

private[heddle] final class BytesView(val buf: Chunk[Byte], val from: Int, val until: Int):
  override def hashCode: Int =
    var h = 1
    var i = from
    while i < until do
      h = 31 * h + (buf(i) & 0xff)
      i += 1
    h

  override def equals(other: Any): Boolean =
    other match
      case k: BytesView =>
        val n = until - from
        if n != k.until - k.from then false
        else
          var i = 0
          while i < n && buf(from + i) == k.buf(k.from + i) do i += 1
          i == n
      case _ => false
end BytesView

private[heddle] object BytesView:
  def view(buf: Chunk[Byte], from: Int, until: Int): BytesView = BytesView(buf, from, until)
  def copy(bytes: Array[Byte]): BytesView                      = BytesView(Chunk.fromArray(bytes), 0, bytes.length)
