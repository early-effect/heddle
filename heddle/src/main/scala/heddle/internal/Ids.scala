package heddle.internal

import java.util.UUID

/** UUID v4 from platform CSPRNG. Do not use `UUID.randomUUID` (needs SecureRandom on JS). */
private[heddle] object Ids:
  def uuid(): UUID =
    val bytes = new Array[Byte](16)
    IdsPlatform.fill(bytes)
    bytes(6) = ((bytes(6) & 0x0f) | 0x40).toByte
    bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
    var hi = 0L
    var lo = 0L
    var i  = 0
    while i < 8 do
      hi = (hi << 8) | (bytes(i) & 0xff)
      i += 1
    while i < 16 do
      lo = (lo << 8) | (bytes(i) & 0xff)
      i += 1
    UUID(hi, lo)
  end uuid
end Ids

