package heddle.internal

import heddle.internal.openssl.Ssl

private[heddle] object IdsPlatform:
  def fill(dst: Array[Byte]): Boolean = Ssl.randomBytes(dst)
