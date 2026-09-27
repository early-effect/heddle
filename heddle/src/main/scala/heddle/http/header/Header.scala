package heddle.http.header

import heddle.internal.Ascii

/** A header field. One parsed from the wire keeps a slice of the request bytes and decodes its value on demand. */
sealed abstract class Header(val name: HeaderName):
  def value: String

  override def equals(other: Any): Boolean =
    other match
      case h: Header => name == h.name && value == h.value
      case _         => false

  override def hashCode: Int = name.hashCode * 31 + value.hashCode

  override def toString: String = s"Header($name, $value)"
end Header

object Header:
  private final class Text(name: HeaderName, val value: String) extends Header(name)

  private final class Slice(name: HeaderName, bytes: Array[Byte], from: Int, until: Int) extends Header(name):
    def value: String = Ascii.string(bytes, from, until)

  def apply(name: HeaderName, value: String): Header =
    Text(name, value)

  def apply(name: String, value: String): Header =
    apply(HeaderName(name), value)

  def origin(scheme: String, host: String, port: Option[Int] = None): Header =
    val skip = port.filterNot(n => (scheme == "http" && n == 80) || (scheme == "https" && n == 443))
    val loc  = skip.fold(s"$scheme://$host")(n => s"$scheme://$host:$n")
    Header(HeaderName.Origin, loc)

  def unapply(h: Header): Some[(HeaderName, String)] = Some((h.name, h.value))

  private[heddle] def slice(name: HeaderName, bytes: Array[Byte], from: Int, until: Int): Header =
    val (a, b) = Ascii.trim(bytes, from, until)
    Ascii.internValue(bytes, a, b) match
      case Some(known) => Text(name, known)
      case None        => Slice(name, bytes, a, b)
end Header
