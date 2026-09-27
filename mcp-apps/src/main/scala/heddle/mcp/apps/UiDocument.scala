package heddle.mcp.apps

/** A view's HTML, rendered from its script (a linked Scala.js bundle), so the server knows the script's CSP hash by
  * construction. The script is inlined: MCP App views have no origin to load it from.
  */
final case class UiDocument private (title: String, script: String):
  /** `</script` inside the bundle would end the element early; `<\/script` means the same to JavaScript. */
  private def safeScript: String = script.replace("</script", "<\\/script")

  /** The CSP hash source for the inline script, as a host puts it in `script-src`. */
  def scriptHash: String = Sha256.cspSource(safeScript)

  def html: String =
    s"""<!doctype html>
       |<html lang="en">
       |<head>
       |<meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1">
       |<title>${UiDocument.escape(title)}</title>
       |</head>
       |<body>
       |<div id="app"></div>
       |<script>$safeScript</script>
       |</body>
       |</html>
       |""".stripMargin
end UiDocument

object UiDocument:
  def apply(title: String, script: String): UiDocument = new UiDocument(title, script)

  private def escape(s: String): String =
    s.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c   => c.toString
    }
end UiDocument
