package heddle.http

import java.net.{URLDecoder, URLEncoder}
import zio.Chunk

final case class Form(fields: Chunk[(String, String)]):
  def get(name: String): Option[String] =
    fields.find(_._1 == name).map(_._2)

  def getAll(name: String): Chunk[String] =
    fields.collect { case (n, v) if n == name => v }

  def isEmpty: Boolean = fields.isEmpty

  def render: String = Form.encode(this)
end Form

object Form:
  val empty: Form = Form(Chunk.empty)

  def apply(pairs: (String, String)*): Form =
    Form(Chunk.fromIterable(pairs))

  def encode(form: Form): String =
    form.fields
      .map { (k, v) =>
        s"${enc(k)}=${enc(v)}"
      }
      .mkString("&")

  def decode(raw: String): Form =
    if raw.isEmpty then empty
    else
      Form(
        Chunk.fromIterable(
          raw
            .split('&')
            .iterator
            .filter(_.nonEmpty)
            .map { pair =>
              val eq = pair.indexOf('=')
              if eq < 0 then dec(pair)        -> ""
              else dec(pair.substring(0, eq)) -> dec(pair.substring(eq + 1))
            }
            .toList
        )
      )

  private def enc(s: String): String =
    URLEncoder.encode(s, "UTF-8")

  private def dec(s: String): String =
    URLDecoder.decode(s, "UTF-8")
end Form
