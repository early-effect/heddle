package heddle.mcp.apps

import heddle.error.HeddleError

/** Why text is not a view's `ui://` resource URI. */
enum UiUriError(val message: String) extends HeddleError:
  case NotUi                             extends UiUriError("a view's uri starts with ui://")
  case Empty                             extends UiUriError("a ui:// uri names a resource after the scheme")
  case BadCharacter(char: Char, at: Int) extends UiUriError(s"'$char' at $at cannot appear in a ui:// uri")
