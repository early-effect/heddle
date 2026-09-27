package heddle.internal.h2

import heddle.error.HpackError

/** The decoder's dynamic table (RFC 7541 §2.3.2), newest entry first. It lives as long as the connection. `size` counts
  * each entry as its name and value plus 32 octets; `limit` is the SETTINGS_HEADER_TABLE_SIZE this endpoint sent.
  */
private[heddle] final case class HpackTable(entries: Vector[(String, String)], size: Int, max: Int, limit: Int):
  def get(index: Int): Option[(String, String)] =
    if index <= 0 then None
    else if index <= HpackTable.StaticSize then Hpack.static(index)
    else entries.lift(index - HpackTable.StaticSize - 1)

  def add(entry: (String, String)): HpackTable =
    val cost = HpackTable.cost(entry)
    if cost > max then HpackTable(Vector.empty, 0, max, limit)
    else HpackTable(entry +: entries, size + cost, max, limit).fit

  def resize(to: Int): Either[HpackError, HpackTable] =
    if to > limit then Left(HpackError.TableTooLarge(to, limit)) else Right(copy(max = to).fit)

  private def fit: HpackTable =
    if size <= max then this
    else
      entries.lastOption match
        case Some(oldest) => HpackTable(entries.init, size - HpackTable.cost(oldest), max, limit).fit
        case None         => HpackTable(Vector.empty, 0, max, limit)
end HpackTable

private[heddle] object HpackTable:
  val StaticSize: Int                = 61
  val DefaultLimit: Int              = 4096
  val empty: HpackTable              = HpackTable(Vector.empty, 0, DefaultLimit, DefaultLimit)
  def cost(e: (String, String)): Int = e._1.length + e._2.length + 32
