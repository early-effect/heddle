package heddle.internal

import java.io.FileInputStream

/** Kernel Native has no OpenSSL yet. `/dev/urandom` is the kernel CSPRNG. */
private[heddle] object IdsPlatform:
  def fill(dst: Array[Byte]): Unit =
    val in = FileInputStream("/dev/urandom")
    try
      var n = 0
      while n < dst.length do
        val got = in.read(dst, n, dst.length - n)
        if got < 0 then throw java.io.IOException("short read from /dev/urandom")
        n += got
    finally in.close()
end IdsPlatform
