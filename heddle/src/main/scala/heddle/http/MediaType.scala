package heddle.http

final class MediaType private (
    val mainType: String,
    val subType: String,
    val charset: Option[String],
    val params: List[(String, String)],
):
  def base: String = s"$mainType/$subType"

  def render: String =
    val cs   = charset.map(c => s"; charset=$c").getOrElse("")
    val rest = params.map((k, v) => s"; $k=$v").mkString
    s"$base$cs$rest"

  def isEventStream: Boolean = mainType == "text" && subType == "event-stream"
  def isJson: Boolean        = subType == "json" || subType.endsWith("+json")
  def isImage: Boolean       = mainType == "image"
  def isVideo: Boolean       = mainType == "video"

  override def equals(other: Any): Boolean =
    other match
      case m: MediaType =>
        mainType == m.mainType && subType == m.subType && charset == m.charset && params.toSet == m.params.toSet
      case _ => false

  override def hashCode: Int    = (mainType, subType, charset, params.toSet).hashCode
  override def toString: String = render
end MediaType

object MediaType:
  val Json: MediaType           = apply("application", "json")
  val JsonUtf8: MediaType       = apply("application", "json", Some("utf-8"))
  val Text: MediaType           = apply("text", "plain")
  val TextUtf8: MediaType       = apply("text", "plain", Some("utf-8"))
  val HtmlUtf8: MediaType       = apply("text", "html", Some("utf-8"))
  val EventStream: MediaType    = apply("text", "event-stream")
  val OctetStream: MediaType    = apply("application", "octet-stream")
  val FormUrlEncoded: MediaType = apply("application", "x-www-form-urlencoded")
  val MultipartForm: MediaType  = apply("multipart", "form-data")
  val JavascriptUtf8: MediaType = apply("text", "javascript", Some("utf-8"))
  val CssUtf8: MediaType        = apply("text", "css", Some("utf-8"))
  val Svg: MediaType            = apply("image", "svg+xml")
  val Png: MediaType            = apply("image", "png")
  val Jpeg: MediaType           = apply("image", "jpeg")
  val Gif: MediaType            = apply("image", "gif")
  val Woff2: MediaType          = apply("font", "woff2")

  def apply(
      mainType: String,
      subType: String,
      charset: Option[String] = None,
      params: List[(String, String)] = Nil,
  ): MediaType =
    val cs = charset.map(asciiLower)
    val ps = params.collect:
      case (k, v) if !k.equalsIgnoreCase("charset") => asciiLower(k) -> v
    new MediaType(asciiLower(mainType), asciiLower(subType), cs, ps)

  def parse(raw: String): Option[MediaType] =
    val s     = raw.trim
    val slash = s.indexOf('/')
    if slash <= 0 || slash == s.length - 1 then None
    else
      val restStart = s.indexOf(';', slash)
      val main      = s.substring(0, slash).trim
      val subEnd    = if restStart < 0 then s.length else restStart
      val sub       = s.substring(slash + 1, subEnd).trim
      if main.isEmpty || sub.isEmpty then None
      else
        var charset: Option[String] = None
        val params                  = List.newBuilder[(String, String)]
        if restStart >= 0 then
          var from = restStart + 1
          while from <= s.length do
            val semi = s.indexOf(';', from)
            val end  = if semi < 0 then s.length else semi
            val seg  = s.substring(from, end).trim
            if seg.nonEmpty then
              val eq = seg.indexOf('=')
              if eq > 0 then
                val k = seg.substring(0, eq).trim
                var v = seg.substring(eq + 1).trim
                if v.length >= 2 && v.charAt(0) == '"' && v.charAt(v.length - 1) == '"' then
                  v = v.substring(1, v.length - 1)
                if k.equalsIgnoreCase("charset") then charset = Some(asciiLower(v))
                else params += (asciiLower(k) -> v)
            from = if semi < 0 then s.length + 1 else semi + 1
        end if
        Some(apply(main, sub, charset, params.result()))
      end if
    end if
  end parse

  def fromExtension(ext: String): MediaType =
    ext.toLowerCase match
      case "html" | "htm" => HtmlUtf8
      case "js" | "mjs"   => JavascriptUtf8
      case "css"          => CssUtf8
      case "json"         => JsonUtf8
      case "map"          => Json
      case "txt" | "md"   => TextUtf8
      case "svg"          => Svg
      case "png"          => Png
      case "jpg" | "jpeg" => Jpeg
      case "gif"          => Gif
      case "woff2"        => Woff2
      case _              => OctetStream

  private def asciiLower(s: String): String =
    val arr = s.toCharArray
    var i   = 0
    while i < arr.length do
      val c = arr(i)
      if c >= 'A' && c <= 'Z' then arr(i) = (c + 32).toChar
      i += 1
    String(arr)
end MediaType
