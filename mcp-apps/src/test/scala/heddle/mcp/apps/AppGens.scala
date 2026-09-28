package heddle.mcp.apps

import zio.test.Gen

object AppGens:
  private val label = Gen.stringBounded(1, 8)(Gen.alphaNumericChar).map(_.toLowerCase)

  val origin: Gen[Any, Origin] =
    val raw =
      for
        scheme <- Gen.elements(Origin.Scheme.values*)
        host   <- Gen.listOfBounded(1, 3)(label).map(_.mkString("."))
        port   <- Gen.option(Gen.int(1, 65535))
      yield s"${scheme.render}://$host${port.fold("")(p => s":$p")}"
    raw.map(Origin.from).collect { case Right(o) => o }

  /** The origins every ask and allowance draws from. The clamp laws are set algebra over origins, so an ask and an
    * allowance must share some, and independently random hostnames never do. `a.test` under every scheme and a second
    * port holds origins that differ only in scheme or port apart. `origin` covers parsing.
    */
  private val universe: List[Origin] = List(
    Origin("https://a.test"),
    Origin("https://a.test:8443"),
    Origin("http://a.test"),
    Origin("wss://a.test"),
    Origin("ws://a.test"),
    Origin("https://api.b.test"),
    Origin("http://localhost:3000"),
    Origin("http://[::1]:8080"),
  )

  private def subset[A](xs: List[A]): Gen[Any, Set[A]] =
    Gen.listOfN(xs.length)(Gen.boolean).map(keep => xs.zip(keep).collect { case (x, true) => x }.toSet)

  private val origins: Gen[Any, Set[Origin]] = subset(universe)

  val network: Gen[Any, Network] = (origins <*> origins <*> origins <*> origins).map(Network(_, _, _, _))

  val permissions: Gen[Any, Set[Permission]] = subset(Permission.values.toList)

  val appOrigin: Gen[Any, AppOrigin] =
    Gen.oneOf(Gen.const(AppOrigin.Opaque), label.map(AppOrigin.Stable(_)))

  val policy: Gen[Any, UiPolicy] =
    (network <*> permissions <*> appOrigin <*> Gen.elements(Border.values*)).map(UiPolicy(_, _, _, _))

  private val admit: Gen[Any, Admit] =
    Gen.oneOf(Gen.const(Admit.AnyOrigin), origins.map(Admit.Only(_)))

  val host: Gen[Any, HostPolicy] =
    ((admit <*> admit <*> admit <*> admit).map(NetworkAllowance(_, _, _, _)) <*> permissions <*>
      Gen.elements(StableOrigins.values*)).map(HostPolicy(_, _, _))

  /** An allowance and one at least as wide: every origin, permission, and stable origin the first admits, and more. */
  val widening: Gen[Any, (HostPolicy, HostPolicy)] =
    for
      narrow <- host
      extra  <- origins
      morePs <- permissions
      mint   <- Gen.boolean
    yield
      def widen(a: Admit): Admit =
        a match
          case Admit.AnyOrigin => Admit.AnyOrigin
          case Admit.Only(os)  => Admit.Only(os ++ extra)
      val n    = narrow.network
      val wide = HostPolicy(
        NetworkAllowance(widen(n.connect), widen(n.resources), widen(n.frames), widen(n.base)),
        narrow.permissions ++ morePs,
        if mint then StableOrigins.Mint else narrow.stable,
      )
      (narrow, wide)
end AppGens
