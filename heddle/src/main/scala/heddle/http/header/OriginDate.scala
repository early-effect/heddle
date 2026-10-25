package heddle.http.header

import java.time.Instant
import zio.*

/** Origin `Date` stamp for responses. One fiber per server publishes the IMF-fixdate once per second. */
private[heddle] final case class OriginDate private (ref: Ref[OriginDate.Stamp]):
  def get: UIO[OriginDate.Stamp] = ref.get

private[heddle] object OriginDate:
  final case class Stamp(value: String, http1Line: Chunk[Byte])

  def stampOf(instant: Instant): Stamp =
    DateStampPlatform.stamp(instant)

  /** Starts the one-second refresh fiber in the caller's scope. The fiber reads [[Clock]] from that environment. */
  def make: ZIO[Scope, Nothing, OriginDate] =
    for
      now <- Clock.instant
      ref <- Ref.make(stampOf(now))
      _   <- (sleepToNextBoundary *> refresh(ref)).forever.forkScoped
    yield OriginDate(ref)

  private def refresh(ref: Ref[Stamp]): UIO[Unit] =
    Clock.instant.flatMap(now => ref.set(stampOf(now)))

  private def sleepToNextBoundary: UIO[Unit] =
    Clock.instant.flatMap { now =>
      val rem = 1000L - (now.toEpochMilli % 1000L)
      Clock.sleep(Duration.fromMillis(if rem == 0L then 1000L else rem))
    }
end OriginDate
