package heddle.brotli

/** RFC 7932 static dictionary. */
private[brotli] object Dict:
  val MinWordLength: Int  = 4
  val MaxWordLength: Int  = 24
  val MaxTransformed: Int = 5 + MaxWordLength + 8

  val OffsetsByLength: Array[Int] =
    Array(
      0, 0, 0, 0, 0, 4096, 9216, 21504, 35840, 44032, 53248, 63488, 74752, 87040, 93696, 100864, 104704, 106752, 108928,
      113536, 115968, 118528, 119872, 121280, 122016,
    )

  val SizeBitsByLength: Array[Int] =
    Array(0, 0, 0, 0, 10, 10, 11, 11, 10, 10, 10, 10, 10, 9, 9, 8, 7, 7, 8, 7, 7, 6, 6, 5, 5)

  val data: Array[Byte] = resourceBytes("dictionary.bin")

  val contextLookup: Array[Int] =
    val raw = resourceBytes("context-lookup.bin")
    val out = Array.ofDim[Int](raw.length / 2)
    var i   = 0
    while i < out.length do
      out(i) = (raw(i * 2) & 0xff) | ((raw(i * 2 + 1) & 0xff) << 8)
      i += 1
    out

  val LookupOffsets: Array[Int] = Array(1024, 1536, 1280, 1536, 0, 256, 768, 512)

  private def resourceBytes(name: String): Array[Byte] =
    val in = getClass.getResourceAsStream(s"/heddle/brotli/$name")
    if in == null then throw IllegalStateException(s"missing resource heddle/brotli/$name")
    try in.readAllBytes()
    finally in.close()
end Dict
