package heddle.mcp.apps.ui

import heddle.mcp.apps.Origin
import zio.json.*
import zio.json.ast.Json

/** What a relay document is told by whoever built it: the one origin whose page may talk to it, and the grant its view
  * runs under. The relay trusts only this, never the `sandbox`, `csp`, or `permissions` in `sandbox-resource-ready`.
  */
final case class RelayConfig(host: Origin, grant: SandboxGrant)

object RelayConfig:
  /** The id of the `<script type="application/json">` element that carries it. */
  val ElementId = "heddle-relay-config"

  given JsonCodec[RelayConfig] =
    JsonCodec(
      Json.encoder.contramap(c => Json.Obj("host" -> Json.Str(c.host.render), "grant" -> UiWire.json(c.grant))),
      Json.decoder.mapOrFail {
        case o: Json.Obj =>
          for
            raw   <- o.get("host").collect { case Json.Str(s) => s }.toRight("no host")
            host  <- Origin.from(raw).left.map(_.message)
            grant <- o.get("grant").toRight("no grant").flatMap(SandboxGrant.strict)
          yield RelayConfig(host, grant)
        case other => Left(s"not a relay config: $other")
      },
    )
end RelayConfig
