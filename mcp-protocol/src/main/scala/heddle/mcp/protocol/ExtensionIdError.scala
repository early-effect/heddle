package heddle.mcp.protocol

/** Why text is not an extension id: `vendor-prefix/name`, each of `A-Z a-z 0-9 - . _` (SEP-1724). */
enum ExtensionIdError(val message: String):
  case NotPrefixSlashName extends ExtensionIdError("an extension id is prefix/name, one slash, both non-empty")
  case BadCharacter(char: Char, at: Int) extends ExtensionIdError(s"'$char' at $at is not one of A-Z a-z 0-9 - . _")
