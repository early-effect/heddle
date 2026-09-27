package heddle.http

/** RFC 9110 §5.6.6 parameters, `; name=token` or `; name="quoted \" string"`, as in `Content-Type` and
  * `Content-Disposition`. Names are case-insensitive and come back lower-cased. `;` inside quotes is part of the value.
  */
private[heddle] object HeaderParams:
  /** The parameters in `raw` from index `from`, in order. A name with no `=` is dropped. */
  def parse(raw: String, from: Int): List[(String, String)] =
    val out = List.newBuilder[(String, String)]
    val n   = raw.length
    var i   = from
    while i < n do
      while i < n && (raw.charAt(i) == ';' || isSpace(raw.charAt(i))) do i += 1
      val nameStart = i
      while i < n && raw.charAt(i) != '=' && raw.charAt(i) != ';' do i += 1
      val name = raw.substring(nameStart, i).trim
      if i < n && raw.charAt(i) == '=' then
        i += 1
        while i < n && isSpace(raw.charAt(i)) do i += 1
        val value =
          if i < n && raw.charAt(i) == '"' then
            val sb = StringBuilder()
            i += 1
            while i < n && raw.charAt(i) != '"' do
              if raw.charAt(i) == '\\' && i + 1 < n then i += 1
              sb += raw.charAt(i)
              i += 1
            i += 1
            sb.result()
          else
            val start = i
            while i < n && raw.charAt(i) != ';' do i += 1
            raw.substring(start, i).trim
        if name.nonEmpty then out += (lower(name) -> value)
      end if
    end while
    out.result()
  end parse

  /** `; name=value`, quoting the value unless it is a non-empty token. */
  def render(name: String, value: String): String =
    if value.nonEmpty && value.forall(isTokenChar) then s"; $name=$value" else s"; $name=${quoted(value)}"

  /** A quoted-string. CR and LF cannot be quoted, so they are written as `%0D` and `%0A` (the WHATWG form encoding). */
  def quoted(value: String): String =
    val sb = StringBuilder("\"")
    value.foreach {
      case '"'  => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\r' => sb ++= "%0D"
      case '\n' => sb ++= "%0A"
      case c    => sb += c
    }
    sb += '"'
    sb.result()
  end quoted

  def lower(s: String): String =
    val arr = s.toCharArray
    var i   = 0
    while i < arr.length do
      val c = arr(i)
      if c >= 'A' && c <= 'Z' then arr(i) = (c + 32).toChar
      i += 1
    String(arr)

  private def isSpace(c: Char): Boolean = c == ' ' || c == '\t'

  private def isTokenChar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || "!#$%&'*+-.^_`|~".contains(c)
end HeaderParams
