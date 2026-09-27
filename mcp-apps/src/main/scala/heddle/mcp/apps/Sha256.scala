package heddle.mcp.apps

import java.nio.charset.StandardCharsets
import java.util.Base64
import zio.Chunk

/** SHA-256 (FIPS 180-4) in plain Scala, so a browser can pin a view's bytes without Node or an async WebCrypto call.
  * `Sha256Spec` checks it against the FIPS vectors and `Sha256OracleSpec`, on the JVM, against `MessageDigest`.
  */
object Sha256:
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

  def digest(bytes: Chunk[Byte]): Chunk[Byte] =
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
    end while
    Chunk.fromArray(h.flatMap(v => Array((v >>> 24).toByte, (v >>> 16).toByte, (v >>> 8).toByte, v.toByte)))
  end digest

  def hex(bytes: Chunk[Byte]): String = digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  /** The CSP hash source for an inline script: `sha256-<base64>`. */
  def cspSource(script: String): String =
    "sha256-" + Base64.getEncoder.encodeToString(
      digest(Chunk.fromArray(script.getBytes(StandardCharsets.UTF_8))).toArray
    )

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
  end padded
end Sha256
