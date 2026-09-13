package heddle.http.header

import heddle.internal.Ascii

final class Header private (
    val name: HeaderName,
    private val interned: String | Null,
    private val bytes: Array[Byte] | Null,
    private val from: Int,
    private val until: Int,
):
  def value: String =
    if interned != null then interned
    else if bytes == null then ""
    else Ascii.string(bytes, from, until)

  override def equals(other: Any): Boolean =
    other match
      case h: Header => name == h.name && value == h.value
      case _         => false

  override def hashCode: Int = name.hashCode * 31 + value.hashCode

  override def toString: String = s"Header($name, $value)"
end Header

object Header:
  def apply(name: HeaderName, value: String): Header =
    new Header(name, value, null, 0, 0)

  def apply(name: String, value: String): Header =
    apply(HeaderName(name), value)

  def origin(scheme: String, host: String, port: Option[Int] = None): Header =
    val skip = port.filterNot(n => (scheme == "http" && n == 80) || (scheme == "https" && n == 443))
    val loc  = skip.fold(s"$scheme://$host")(n => s"$scheme://$host:$n")
    Header(HeaderName.Origin, loc)

  def unapply(h: Header): Some[(HeaderName, String)] = Some((h.name, h.value))

  private[heddle] def slice(name: HeaderName, bytes: Array[Byte], from: Int, until: Int): Header =
    val (a, b) = Ascii.trim(bytes, from, until)
    val hit    = Ascii.internValue(bytes, from, until)
    if hit != null then new Header(name, hit, null, 0, 0)
    else new Header(name, null, bytes, a, b)
end Header
