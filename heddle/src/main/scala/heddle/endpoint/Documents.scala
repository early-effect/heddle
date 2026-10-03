package heddle.endpoint

import heddle.http.MediaType
import zio.Chunk

opaque type Text = String
object Text:
  def apply(value: String): Text           = value
  extension (text: Text) def value: String = text
  given BodyCodec[Text]                    = BodyCodec.text[Text](MediaType.TextUtf8)(identity, Text.apply)

opaque type Html = String
object Html:
  def apply(value: String): Html           = value
  extension (html: Html) def value: String = html
  given BodyCodec[Html]                    = BodyCodec.text[Html](MediaType.HtmlUtf8)(identity, Html.apply)

opaque type Javascript = String
object Javascript:
  def apply(value: String): Javascript             = value
  extension (script: Javascript) def value: String = script
  given BodyCodec[Javascript] = BodyCodec.text[Javascript](MediaType.JavascriptUtf8)(identity, Javascript.apply)

opaque type Css = String
object Css:
  def apply(value: String): Css          = value
  extension (css: Css) def value: String = css
  given BodyCodec[Css]                   = BodyCodec.text[Css](MediaType.CssUtf8)(identity, Css.apply)

opaque type Svg = String
object Svg:
  def apply(value: String): Svg          = value
  extension (svg: Svg) def value: String = svg
  given BodyCodec[Svg]                   = BodyCodec.text[Svg](MediaType.Svg)(identity, Svg.apply)

opaque type Octet = Chunk[Byte]
object Octet:
  def apply(bytes: Chunk[Byte]): Octet            = bytes
  extension (octet: Octet) def value: Chunk[Byte] = octet
  given BodyCodec[Octet]                          = BodyCodec.bytes[Octet](MediaType.OctetStream)(identity, Octet.apply)
