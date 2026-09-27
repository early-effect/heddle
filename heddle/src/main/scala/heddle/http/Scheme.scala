package heddle.http

enum Scheme(val render: String, val defaultPort: Int):
  case Http  extends Scheme("http", 80)
  case Https extends Scheme("https", 443)
  case Ws    extends Scheme("ws", 80)
  case Wss   extends Scheme("wss", 443)

  def secure: Boolean = this == Https || this == Wss

object Scheme:
  def parse(raw: String): Option[Scheme] =
    values.find(_.render.equalsIgnoreCase(raw))
