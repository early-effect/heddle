package example

import heddle.*

final case class Show(id: Int, title: String, remaining: Int) derives Schema, JsonCodec

final case class Party(showId: Int, size: Int) derives Schema, JsonCodec

final case class PartySeated(
    orderId: Int,
    seats: List[String],
    totalCents: Int,
    pickupCode: String,
) derives Schema,
      JsonCodec

final case class NewHold(showId: Int, size: Int) derives Schema, JsonCodec

final case class Hold(id: Int, showId: Int, seats: List[String]) derives Schema, JsonCodec

final case class NewOrder(holdId: Int) derives Schema, JsonCodec

final case class Order(
    id: Int,
    showId: Int,
    seats: List[String],
    totalCents: Int,
    pickupCode: String,
) derives Schema,
      JsonCodec

final case class NotFound(message: String) derives Schema, JsonCodec

final case class Me(sub: String, scopes: List[String]) derives Schema, JsonCodec

enum SeatingError derives Schema, JsonCodec:
  case SoldOut(showId: Int)
  case NoBlock(showId: Int, size: Int)
