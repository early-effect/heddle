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

  /** A small pool, so asks and allowances overlap often enough to exercise both sides of every clamp. */
  private val pool: Gen[Any, List[Origin]] = Gen.listOfN(6)(origin)

  private def subset[A](xs: List[A]): Gen[Any, Set[A]] =
    Gen.listOfN(xs.length)(Gen.boolean).map(keep => xs.zip(keep).collect { case (x, true) => x }.toSet)

  private val originSets: Gen[Any, (List[Origin], Set[Origin])] = pool.flatMap(p => subset(p).map(p -> _))

  val network: Gen[Any, Network] =
    pool.flatMap(p => (subset(p) <*> subset(p) <*> subset(p) <*> subset(p)).map(Network(_, _, _, _)))

  val permissions: Gen[Any, Set[Permission]] = subset(Permission.values.toList)

  val appOrigin: Gen[Any, AppOrigin] =
    Gen.oneOf(Gen.const(AppOrigin.Opaque), label.map(AppOrigin.Stable(_)))

  val policy: Gen[Any, UiPolicy] =
    (network <*> permissions <*> appOrigin <*> Gen.elements(Border.values*)).map(UiPolicy(_, _, _, _))

  private val admit: Gen[Any, Admit] =
    Gen.oneOf(Gen.const(Admit.AnyOrigin), originSets.map((_, s) => Admit.Only(s)))

  val host: Gen[Any, HostPolicy] =
    ((admit <*> admit <*> admit <*> admit).map(NetworkAllowance(_, _, _, _)) <*> permissions <*>
      Gen.elements(StableOrigins.values*)).map(HostPolicy(_, _, _))

  /** An allowance and one at least as wide: every origin, permission, and stable origin the first admits, and more. */
  val widening: Gen[Any, (HostPolicy, HostPolicy)] =
    for
      narrow <- host
      extra  <- pool
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
