package heddle.docs.ui

/** A Scala string literal of `value`, for source the transcript generator writes. */
object ScalaLiteral:
  def apply(value: String): String =
    val escaped = value.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case char => char.toString
    }
    "\"" + escaped + "\""
