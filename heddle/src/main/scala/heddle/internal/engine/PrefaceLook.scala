package heddle.internal.engine

/** What the bytes already buffered say about an HTTP/2 preface, without reading more. */
private[heddle] enum PrefaceLook:
  /** A proper prefix of the preface, or a diverged block that is not finished and still under the header cap. */
  case NeedMore

  /** The 24-byte connection preface. */
  case Http2

  /** Not the preface, and a header block is already complete. */
  case Http1

  /** Not the preface, no blank line, and the cap is already buffered. */
  case HeadersTooLarge
end PrefaceLook

private[heddle] object PrefaceLook:
  /** `length` bytes at `offset` in `raw`. A matching preface prefix is never HTTP/1, even where it contains `\r\n\r\n`.
    */
  def classify(
      raw: Array[Byte],
      offset: Int,
      length: Int,
      preface: Array[Byte],
      maxHeader: Int,
  ): PrefaceLook =
    val matched = prefixLength(raw, offset, length, preface)
    if matched == preface.length then PrefaceLook.Http2
    else if matched == length then PrefaceLook.NeedMore
    else if hasDoubleCrlf(raw, offset, length) then PrefaceLook.Http1
    else if length >= maxHeader then PrefaceLook.HeadersTooLarge
    else PrefaceLook.NeedMore

  /** How many leading bytes match. `preface.length` is a full preface. `length` is a proper prefix. Anything else
    * diverged.
    */
  private def prefixLength(raw: Array[Byte], offset: Int, length: Int, preface: Array[Byte]): Int =
    val n = length.min(preface.length)
    var i = 0
    while i < n && raw(offset + i) == preface(i) do i += 1
    if i == preface.length then preface.length
    else if i == length then length
    else i

  private def hasDoubleCrlf(raw: Array[Byte], offset: Int, length: Int): Boolean =
    val end = offset + length
    var i   = offset
    while i + 3 < end && !(raw(i) == '\r' && raw(i + 1) == '\n' && raw(i + 2) == '\r' && raw(i + 3) == '\n') do i += 1
    i + 3 < end
end PrefaceLook
