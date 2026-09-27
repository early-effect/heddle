package heddle.internal

import java.security.SecureRandom

private[heddle] object IdsPlatform:
  private val rng = new SecureRandom()

  def fill(dst: Array[Byte]): Boolean =
    rng.nextBytes(dst)
    true
