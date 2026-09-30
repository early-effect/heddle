package heddle.mcp.apps.ui

import heddle.mcp.apps.Origin
import zio.json.*

/** What a relay document is told by whoever built it: the one origin whose page may talk to it, and the grant its view
  * runs under. The relay trusts only this, never the `sandbox`, `csp`, or `permissions` in `sandbox-resource-ready`.
  */
final case class RelayConfig(host: Origin, grant: SandboxGrant) derives JsonCodec

object RelayConfig:
  /** The id of the `<script type="application/json">` element that carries it. */
  val ElementId = "heddle-relay-config"

  /** A relay runs under exactly the grant it was given, so one it cannot honor whole is not a config. */
  private given JsonCodec[SandboxGrant] = SandboxGrant.strict
