package example

import zio.*

final class BoxOffice(
    shows: Ref[Map[Int, String]],
    seats: Ref[Map[Int, Vector[Boolean]]],
    holds: Ref[Map[Int, Hold]],
    orders: Ref[Map[Int, Order]],
    nextHold: Ref[Int],
    nextOrder: Ref[Int],
    nextCode: Ref[Int],
):
  def listShows: UIO[List[Show]] =
    for
      titles <- shows.get
      rows   <- seats.get
    yield titles.toList.sortBy(_._1).map { (id, title) =>
      Show(id, title, rows.getOrElse(id, Vector.empty).count(identity))
    }

  def get(id: Int): IO[NotFound, Show] =
    listShows.flatMap(ss => ZIO.fromOption(ss.find(_.id == id)).orElseFail(NotFound(s"show $id")))

  def remainingSeats(id: Int): IO[NotFound, List[String]] =
    for
      _   <- get(id)
      row <- seats.get.map(_.getOrElse(id, Vector.empty))
    yield row.zipWithIndex.collect { case (true, i) => BoxOffice.label(i) }.toList

  def holdBlock(showId: Int, size: Int): IO[SeatingError, List[String]] =
    if size <= 0 then ZIO.fail(SeatingError.NoBlock(showId, size))
    else
      for
        show  <- get(showId).orElseFail(SeatingError.SoldOut(showId))
        _     <- ZIO.fail(SeatingError.SoldOut(showId)).when(show.remaining < size)
        block <- seats
          .modify { rows =>
            val row = rows.getOrElse(showId, Vector.empty)
            BoxOffice.findBlock(row, size) match
              case None =>
                (None, rows)
              case Some(start) =>
                val next = row.zipWithIndex.map { (free, i) =>
                  if i >= start && i < start + size then false else free
                }
                (Some((start until start + size).map(BoxOffice.label).toList), rows.updated(showId, next))
          }
          .someOrFail(SeatingError.NoBlock(showId, size))
      yield block

  def hold(in: NewHold): IO[SeatingError, Hold] =
    for
      block <- holdBlock(in.showId, in.size)
      id    <- nextHold.getAndUpdate(_ + 1)
      h = Hold(id, in.showId, block)
      _ <- holds.update(_ + (id -> h))
    yield h

  def quote(seats: List[String]): UIO[Int] =
    ZIO.succeed(seats.length * BoxOffice.centsPerSeat)

  def mint: UIO[String] =
    nextCode.getAndUpdate(_ + 1).map(n => f"P$n%04d")

  def record(showId: Int, seated: List[String], total: Int, code: String): UIO[Order] =
    nextOrder.getAndUpdate(_ + 1).flatMap { id =>
      val order = Order(id, showId, seated, total, code)
      orders.update(_ + (id -> order)).as(order)
    }

  def seat(party: Party): IO[SeatingError, PartySeated] =
    for
      show          <- get(party.showId).orElseFail(SeatingError.SoldOut(party.showId))
      _             <- ZIO.fail(SeatingError.SoldOut(party.showId)).when(show.remaining < party.size)
      seated        <- holdBlock(party.showId, party.size)
      (total, code) <- quote(seated).zipPar(mint)
      order         <- record(party.showId, seated, total, code)
    yield PartySeated(order.id, order.seats, order.totalCents, order.pickupCode)

  def orderFromHold(in: NewOrder): IO[NotFound, Order] =
    for
      h <- holds
        .modify { m =>
          m.get(in.holdId) match
            case None    => (None, m)
            case Some(h) => (Some(h), m - in.holdId)
        }
        .someOrFail(NotFound(s"hold ${in.holdId}"))
      (total, code) <- quote(h.seats).zipPar(mint)
      order         <- record(h.showId, h.seats, total, code)
    yield order

  def pickup(code: String): IO[NotFound, Order] =
    orders.get.flatMap { m =>
      ZIO.fromOption(m.values.find(_.pickupCode == code)).orElseFail(NotFound(s"pickup $code"))
    }
end BoxOffice

object BoxOffice:
  val centsPerSeat = 2500

  def label(index: Int): String = s"A${index + 1}"

  def findBlock(row: Vector[Boolean], size: Int): Option[Int] =
    row.indices.find { start =>
      start + size <= row.length && (start until start + size).forall(row)
    }

  def seed: UIO[BoxOffice] =
    for
      shows <- Ref.make(Map(1 -> "Evening bill", 2 -> "Matinee", 3 -> "Late bill"))
      seats <- Ref.make(
        Map(
          1 -> Vector.fill(12)(true),
          2 -> Vector.fill(8)(true),
          3 -> Vector(true, false, true, false),
        )
      )
      holds     <- Ref.make(Map.empty[Int, Hold])
      orders    <- Ref.make(Map.empty[Int, Order])
      nextHold  <- Ref.make(1)
      nextOrder <- Ref.make(1)
      nextCode  <- Ref.make(1001)
    yield BoxOffice(shows, seats, holds, orders, nextHold, nextOrder, nextCode)
end BoxOffice
