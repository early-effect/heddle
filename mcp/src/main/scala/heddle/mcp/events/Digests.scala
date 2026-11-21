package heddle.mcp.events

import zio.Chunk

import java.nio.charset.StandardCharsets

/** SHA-256 (FIPS 180-4) and HMAC-SHA256. Pure Scala so JS and Native can sign without `javax.crypto`. `var` stays
  * inside the compression loop.
  */
private[events] object Digests:
  private val K: Array[Int] = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98,
    0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8,
    0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819,
    0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2,
  )

  def sha256(bytes: Chunk[Byte]): Chunk[Byte] =
    val h   = Array(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
    val msg = padded(bytes)
    val w   = new Array[Int](64)
    var off = 0
    while off < msg.length do
      var t = 0
      while t < 16 do
        val i = off + t * 4
        w(t) = (msg(i) & 0xff) << 24 | (msg(i + 1) & 0xff) << 16 | (msg(i + 2) & 0xff) << 8 | (msg(i + 3) & 0xff)
        t += 1
      while t < 64 do
        val s0 = Integer.rotateRight(w(t - 15), 7) ^ Integer.rotateRight(w(t - 15), 18) ^ (w(t - 15) >>> 3)
        val s1 = Integer.rotateRight(w(t - 2), 17) ^ Integer.rotateRight(w(t - 2), 19) ^ (w(t - 2) >>> 10)
        w(t) = w(t - 16) + s0 + w(t - 7) + s1
        t += 1
      var (a, b, c, d, e, f, g, hh) = (h(0), h(1), h(2), h(3), h(4), h(5), h(6), h(7))
      t = 0
      while t < 64 do
        val s1  = Integer.rotateRight(e, 6) ^ Integer.rotateRight(e, 11) ^ Integer.rotateRight(e, 25)
        val ch  = (e & f) ^ (~e & g)
        val t1  = hh + s1 + ch + K(t) + w(t)
        val s0  = Integer.rotateRight(a, 2) ^ Integer.rotateRight(a, 13) ^ Integer.rotateRight(a, 22)
        val maj = (a & b) ^ (a & c) ^ (b & c)
        val t2  = s0 + maj
        hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2
        t += 1
      h(0) += a; h(1) += b; h(2) += c; h(3) += d; h(4) += e; h(5) += f; h(6) += g; h(7) += hh
      off += 64
    Chunk.fromArray(h.flatMap(v => Array((v >>> 24).toByte, (v >>> 16).toByte, (v >>> 8).toByte, v.toByte)))

  /** HMAC-SHA256. A key longer than the block is hashed first. */
  def hmacSha256(key: Chunk[Byte], message: Chunk[Byte]): Chunk[Byte] =
    val block = 64
    val k     = if key.length > block then sha256(key) else key
    val ipad  = Array.fill(block)(0x36.toByte)
    val opad  = Array.fill(block)(0x5c.toByte)
    var i     = 0
    while i < k.length do
      ipad(i) = (ipad(i) ^ k(i)).toByte
      opad(i) = (opad(i) ^ k(i)).toByte
      i += 1
    sha256(Chunk.fromArray(opad) ++ sha256(Chunk.fromArray(ipad) ++ message))

  def hex(bytes: Chunk[Byte]): String =
    val out = new StringBuilder(bytes.length * 2)
    bytes.foreach { b =>
      val v = b & 0xff
      out.append(Hex(v >>> 4)).append(Hex(v & 0x0f))
    }
    out.toString

  def utf8(text: String): Chunk[Byte] = Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  /** Equal length compared in full. A different length returns before the bytes, which is an accepted leak. */
  def sameBytes(left: Chunk[Byte], right: Chunk[Byte]): Boolean =
    if left.length != right.length then false
    else
      var diff = 0
      var i    = 0
      while i < left.length do
        diff |= (left(i) ^ right(i)) & 0xff
        i += 1
      diff == 0

  private val Hex = "0123456789abcdef"

  private def padded(bytes: Chunk[Byte]): Array[Byte] =
    val bitLen = bytes.length.toLong * 8
    val total  = ((bytes.length + 9 + 63) / 64) * 64
    val out    = new Array[Byte](total)
    bytes.copyToArray(out)
    out(bytes.length) = 0x80.toByte
    var i = 0
    while i < 8 do
      out(total - 1 - i) = (bitLen >>> (8 * i)).toByte
      i += 1
    out
end Digests
