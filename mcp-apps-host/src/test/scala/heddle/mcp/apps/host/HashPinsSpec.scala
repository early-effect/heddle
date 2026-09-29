package heddle.mcp.apps.host

import heddle.mcp.apps.UiUri
import zio.*
import zio.test.*

object HashPinsSpec extends ZIOSpecDefault:
  private val key   = PinKey(ServerName("counter"), UiUri("ui://early-effect/counter"))
  private val bytes = Digest.of("<html></html>")
  private val other = Digest.of("<html>no</html>")

  def spec = suite("HashPins.durable")(
    test("the first bytes are pinned, the same bytes stay, and a change waits for accept"):
      for
        file    <- MemoryFile.make(None)
        pins    <- ZIO.service[HashPins].provide(HashPins.durable(file))
        first   <- pins.check(key, bytes)
        same    <- pins.check(key, bytes)
        changed <- pins.check(key, other)
        stored  <- file.read
        _       <- pins.accept(key, other)
        after   <- pins.check(key, other)
        kept    <- file.read
      yield assertTrue(
        first == PinCheck.FirstSeen,
        same == PinCheck.Same,
        changed == PinCheck.Changed(bytes),
        stored.flatMap(Pins.decode(_).toOption).exists(_.get(key).contains(bytes)),
        after == PinCheck.Same,
        kept.flatMap(Pins.decode(_).toOption).exists(_.get(key).contains(other)),
      )
    ,
    test("a refused write leaves the pin where it was"):
      for
        file      <- MemoryFile.make(None)
        pins      <- ZIO.service[HashPins].provide(HashPins.durable(file))
        _         <- file.refuse.set(true)
        wrote     <- pins.check(key, bytes).either
        untouched <- file.read
        _         <- file.refuse.set(false)
        again     <- pins.check(key, bytes)
      yield assertTrue(
        wrote == Left(PinFileError.Unwritable("refused")),
        untouched.isEmpty,
        again == PinCheck.FirstSeen,
      )
    ,
    test("a missing file and a blank file both open empty"):
      for
        missing <- MemoryFile.make(None)
        blank   <- MemoryFile.make(Some("  \n"))
        a       <- ZIO.service[HashPins].provide(HashPins.durable(missing))
        b       <- ZIO.service[HashPins].provide(HashPins.durable(blank))
        seenA   <- a.check(key, bytes)
        seenB   <- b.check(key, bytes)
      yield assertTrue(seenA == PinCheck.FirstSeen, seenB == PinCheck.FirstSeen)
    ,
    test("a file that is not pins does not open"):
      for
        file <- MemoryFile.make(Some("{"))
        err  <- ZIO.service[HashPins].provide(HashPins.durable(file)).flip
      yield assertTrue(err == PinFileError.Corrupt("not json"))
    ,
    test("a repeated row does not open"):
      val row = Pins.encode(Map(key -> bytes))
      val dup = row.dropRight(1) + "," + row.drop(1)
      for
        file <- MemoryFile.make(Some(dup))
        err  <- ZIO.service[HashPins].provide(HashPins.durable(file)).flip
      yield assertTrue(err match
        case PinFileError.Corrupt(detail) => detail.contains("repeats")
        case _                            => false
      )
    ,
  )

  private final class MemoryFile(body: Ref[Option[String]], val refuse: Ref[Boolean]) extends PinFile:
    def read: IO[PinFileError, Option[String]] = body.get
    def write(text: String): IO[PinFileError, Unit] =
      ZIO.ifZIO(refuse.get)(ZIO.fail(PinFileError.Unwritable("refused")), body.set(Some(text)))

  private object MemoryFile:
    def make(text: Option[String]): UIO[MemoryFile] =
      (Ref.make(text) <*> Ref.make(false)).map(MemoryFile(_, _))
end HashPinsSpec
