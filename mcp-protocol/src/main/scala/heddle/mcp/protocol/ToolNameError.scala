package heddle.mcp.protocol

/** Why text is not a tool name: 1 to 128 of `A-Z a-z 0-9 _ - .` (SEP-986). */
enum ToolNameError(val message: String):
  case Empty                extends ToolNameError("a tool name is 1 to 128 characters, and this is empty")
  case TooLong(length: Int) extends ToolNameError(s"a tool name is at most 128 characters, and this is $length")
  case BadCharacter(char: Char, at: Int) extends ToolNameError(s"'$char' at $at is not one of A-Z a-z 0-9 _ - .")
