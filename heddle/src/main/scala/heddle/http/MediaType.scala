package heddle.http

final class MediaType private (
    val mainType: String,
    val subType: String,
    val charset: Option[String],
    val params: List[(String, String)],
):
  def base: String = s"$mainType/$subType"

  def render: String =
    val cs   = charset.map(HeaderParams.render("charset", _)).getOrElse("")
    val rest = params.map(HeaderParams.render).mkString
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
    val cs = charset.map(HeaderParams.lower)
    val ps = params.collect:
      case (k, v) if !k.equalsIgnoreCase("charset") => HeaderParams.lower(k) -> v
    new MediaType(HeaderParams.lower(mainType), HeaderParams.lower(subType), cs, ps)
  end apply

  def parse(raw: String): Option[MediaType] =
    val s         = raw.trim
    val slash     = s.indexOf('/')
    val restStart = s.indexOf(';', slash.max(0))
    val main      = if slash <= 0 then "" else s.substring(0, slash).trim
    val sub = if slash <= 0 then "" else s.substring(slash + 1, if restStart < 0 then s.length else restStart).trim
    Option.when(main.nonEmpty && sub.nonEmpty) {
      val params = if restStart < 0 then Nil else HeaderParams.parse(s, restStart + 1)
      apply(main, sub, params.collectFirst { case ("charset", v) => v }, params)
    }
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
end MediaType
