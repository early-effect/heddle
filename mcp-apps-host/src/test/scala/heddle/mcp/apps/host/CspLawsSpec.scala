package heddle.mcp.apps.host

import heddle.mcp.apps.{AppGens, Network, ScriptHash}
import heddle.mcp.apps.ui.SandboxGrant
import zio.*
import zio.test.*

object CspLawsSpec extends ZIOSpecDefault:
  private val relay = ScriptHash.of("relay")

  private val scripts: Gen[Any, Csp.Scripts] =
    Gen.listOfBounded(0, 3)(Gen.alphaNumericString).map(ss => Csp.Scripts.of(Chunk.fromIterable(ss.map(ScriptHash.of))))

  private val grant: Gen[Any, SandboxGrant] = (AppGens.network <*> AppGens.permissions).map(SandboxGrant(_, _))

  /** The policy as directive name to sources, in header order. */
  private def directives(csp: Csp): List[(String, List[String])] =
    csp.header.split("; ").toList.map(_.split(' ').toList).collect { case name :: sources => name -> sources }

  private def sources(csp: Csp, name: String): List[String] =
    directives(csp).collectFirst { case (`name`, s) => s }.getOrElse(Nil)

  private def rendered(os: Set[heddle.mcp.apps.Origin]): List[String] = os.toList.map(_.render).sorted

  private val resourceDirectives = List("script-src", "style-src", "img-src", "media-src", "font-src")

  def spec = suite("CSP laws")(
    test("the directives are a fixed set in a fixed order, whatever was granted"):
      check(grant, scripts) { (g, s) =>
        val names = directives(Csp.compile(g, s, relay)).map(_._1)
        val font  = if g.network.resources.nonEmpty then List("font-src") else Nil
        assertTrue(
          names == List("default-src", "script-src", "style-src", "img-src", "media-src") ++ font ++
            List("connect-src", "frame-src", "base-uri", "object-src")
        )
      }
    ,
    test("connect, frame, and base list exactly the granted origins, or 'none' when there are none"):
      check(grant, scripts) { (g, s) =>
        val csp                                     = Csp.compile(g, s, relay)
        def expect(os: Set[heddle.mcp.apps.Origin]) = if os.isEmpty then List("'none'") else rendered(os)
        assertTrue(
          sources(csp, "connect-src") == expect(g.network.connect),
          sources(csp, "frame-src") == expect(g.network.frames),
          sources(csp, "base-uri") == expect(g.network.base),
          sources(csp, "default-src") == List("'none'"),
          sources(csp, "object-src") == List("'none'"),
        )
      }
    ,
    test("resource origins go to every resource directive, and nowhere else"):
      check(grant, scripts) { (g, s) =>
        val csp       = Csp.compile(g, s, relay)
        val resources = rendered(g.network.resources)
        val origins   = (name: String) => sources(csp, name).filter(_.contains("://"))
        assertTrue(
          resourceDirectives.filter(n => n != "font-src" || resources.nonEmpty).forall(n => origins(n) == resources)
        )
      }
    ,
    test("every origin anywhere in the policy was granted for that directive"):
      check(grant, scripts) { (g, s) =>
        val n       = g.network
        val allowed = Map(
          "connect-src" -> n.connect,
          "frame-src"   -> n.frames,
          "base-uri"    -> n.base,
        ).withDefaultValue(n.resources)
        val stray = directives(Csp.compile(g, s, relay)).flatMap { (name, ss) =>
          ss.filter(_.contains("://")).filterNot(o => allowed(name).exists(_.render == o))
        }
        assertTrue(stray.isEmpty)
      }
    ,
    test("a hashed view runs only the relay and its own scripts; nothing turns on eval, self, or a wildcard"):
      check(grant, Gen.listOfBounded(1, 3)(Gen.alphaNumericString)) { (g, ss) =>
        val hashes = Chunk.fromIterable(ss.map(ScriptHash.of))
        val csp    = Csp.compile(g, Csp.Scripts.of(hashes), relay)
        val tokens = directives(csp).flatMap(_._2)
        assertTrue(
          sources(csp, "script-src").filterNot(_.contains("://")) == (relay +: hashes).distinct.map(_.source).toList,
          !tokens.exists(t => t == "'unsafe-eval'" || t == "'self'" || t == "*" || t.contains("*")),
          !sources(csp, "script-src").contains("'unsafe-inline'"),
        )
      }
    ,
    test("a view whose server declared no hashes gets the spec's 'unsafe-inline' and no hash at all"):
      check(grant) { g =>
        val csp = Csp.compile(g, Csp.Scripts.AnyInline, relay)
        assertTrue(sources(csp, "script-src").filterNot(_.contains("://")) == List("'unsafe-inline'"))
      }
    ,
    test("an isolated, hashed view gets exactly the closed policy"):
      val view = ScriptHash.of("view")
      val csp  = Csp.compile(SandboxGrant(Network.isolated), Csp.Scripts.of(Chunk(view)), relay)
      assertTrue(
        csp.header ==
          s"default-src 'none'; script-src ${relay.source} ${view.source}; style-src 'unsafe-inline'; img-src data:; " +
          "media-src data:; connect-src 'none'; frame-src 'none'; base-uri 'none'; object-src 'none'"
      ),
  ) @@ TestAspect.timeout(60.seconds)
end CspLawsSpec
