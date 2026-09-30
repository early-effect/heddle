package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import java.time.Instant
import zio.*
import zio.test.*

/** What the audit keeps, and what it tells whoever follows it. */
object AuditSpec extends ZIOSpecDefault:
  private val actions   = Gen.elements(Action.Mount, Action.Navigated, Action.Teardown, Action.Notification("x"))
  private val decisions = Gen.elements(Decision.Allowed, Decision.DroppedAfterNavigate, Decision.TornDown("gone"))
  private val events    = (Gen.long(0, 1000) <*> actions <*> decisions).map((n, action, decision) =>
    AuditEvent(Instant.EPOCH, ServerName("counter"), UiUri("ui://counter/view"), Generation(n), action, decision)
  )

  private def audit(capacity: Int): URIO[Scope, Audit] = Audit.layer(capacity).build.map(_.get[Audit])

  def spec = suite("Audit")(
    test("a follower sees every event recorded once it has subscribed, in order, before its stream ever runs") {
      check(Gen.chunkOf(events)) { recorded =>
        ZIO.scoped(
          for
            log    <- audit(10000)
            follow <- log.events
            _      <- ZIO.foreachDiscard(recorded)(log.record)
            seen   <- follow.take(recorded.size.toLong).runCollect
          yield assertTrue(seen == recorded)
        )
      }
    },
    test("history is the last `capacity` events, oldest first") {
      check(Gen.int(1, 8), Gen.chunkOf(events)) { (capacity, recorded) =>
        ZIO.scoped(
          for
            log  <- audit(capacity)
            _    <- ZIO.foreachDiscard(recorded)(log.record)
            kept <- log.history
          yield assertTrue(kept == recorded.takeRight(capacity))
        )
      }
    },
  ) @@ TestAspect.timeout(60.seconds)
end AuditSpec
