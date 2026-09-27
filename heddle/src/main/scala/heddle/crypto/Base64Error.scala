package heddle.crypto

import heddle.error.HeddleError

/** Why text is not unpadded or padded base64url (RFC 4648 §5). */
enum Base64Error(val message: String) extends HeddleError:
  case BadCharacter(char: Char, at: Int) extends Base64Error(s"'$char' at $at is not a base64url character")
  case BadLength(length: Int)            extends Base64Error(s"$length base64url characters cannot encode whole bytes")
