package heddle.internal

import zio.Chunk

/** CRC-32 (IEEE 802.3), as gzip's trailer uses it. */
private[heddle] object Crc32:
  private val table: Array[Int] =
    Array.tabulate(256) { n =>
      (0 until 8).foldLeft(n)((c, _) => if (c & 1) != 0 then 0xedb88320 ^ (c >>> 1) else c >>> 1)
    }

  /** Continues `crc` (0 to start) over `bytes`. */
  def update(crc: Int, bytes: Chunk[Byte]): Int =
    ~bytes.foldLeft(~crc)((c, b) => table((c ^ b) & 0xff) ^ (c >>> 8))
