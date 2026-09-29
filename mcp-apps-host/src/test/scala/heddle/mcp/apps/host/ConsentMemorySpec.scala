package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import heddle.mcp.protocol.ToolName
import zio.*
import zio.json.ast.Json
import zio.test.*

/** What the session remembers: `AllowForSession`, per server, view, and tool, whichever gate asked. */
object ConsentMemorySpec extends ZIOSpecDefault:
  private val servers  = Gen.elements(ServerName("counter"), ServerName("seats"))
  private val views    = Gen.elements(UiUri("ui://counter/view"), UiUri("ui://seats/view"))
  private val tools    = Gen.elements(ToolName("inc"), ToolName("reset"))
  private val requests =
    (servers <*> views <*> tools).map((s, v, t) => ConsentRequest(s, v, t, Json.Obj()))
  private val outcomes = Gen.fromIterable(ConsentOutcome.values)

  /** An `ask` that answers `outcome` and counts how often it was asked. */
  private def asking(outcome: ConsentOutcome): UIO[(ConsentRequest => UIO[ConsentOutcome], Ref[Int])] =
    Ref.make(0).map(n => ((_: ConsentRequest) => n.update(_ + 1).as(outcome), n))

  def spec = suite("ConsentMemory")(
    test("once the user allows a call for the session, the same call is allowed without asking again") {
      check(requests) { request =>
        for
          memory       <- ConsentMemory.make
          (ask, times) <- asking(ConsentOutcome.AllowForSession)
          gate = memory.gate(ask)
          first  <- gate.decide(request)
          second <- gate.decide(request.copy(arguments = Json.Obj("n" -> Json.Num(2))))
          n      <- times.get
        yield assertTrue(first == ConsentOutcome.AllowForSession, second == ConsentOutcome.AllowForSession, n == 1)
      }
    },
    test("any other answer is not remembered: the next call asks again") {
      checkAll(outcomes.filter(_ != ConsentOutcome.AllowForSession)) { outcome =>
        check(requests) { request =>
          for
            memory       <- ConsentMemory.make
            (ask, times) <- asking(outcome)
            gate = memory.gate(ask)
            _ <- gate.decide(request)
            _ <- gate.decide(request)
            n <- times.get
          yield assertTrue(n == 2)
        }
      }
    },
    test("what is remembered is that server, view, and tool, and nothing else") {
      check(requests, requests) { (allowed, other) =>
        for
          memory        <- ConsentMemory.make
          (ask, _)      <- asking(ConsentOutcome.AllowForSession)
          (never, asks) <- asking(ConsentOutcome.Rejected)
          _             <- memory.gate(ask).decide(allowed)
          answer        <- memory.gate(never).decide(other)
          n             <- asks.get
          same = (allowed.server, allowed.view, allowed.tool) == (other.server, other.view, other.tool)
        yield assertTrue(
          answer == (if same then ConsentOutcome.AllowForSession else ConsentOutcome.Rejected),
          n == (if same then 0 else 1),
        )
      }
    },
    test("gates that share a memory share what the user allowed, and each asks through its own dialog") {
      check(requests) { request =>
        for
          memory              <- ConsentMemory.make
          (here, askedHere)   <- asking(ConsentOutcome.AllowForSession)
          (there, askedThere) <- asking(ConsentOutcome.Rejected)
          _                   <- memory.gate(here).decide(request)
          answer              <- memory.gate(there).decide(request)
          a                   <- askedHere.get
          b                   <- askedThere.get
        yield assertTrue(answer == ConsentOutcome.AllowForSession, a == 1, b == 0)
      }
    },
  ) @@ TestAspect.timeout(60.seconds)
end ConsentMemorySpec
