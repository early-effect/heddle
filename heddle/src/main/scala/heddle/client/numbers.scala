package heddle.client

import neotype.*

/** In-flight connections to one host. */
type ConnectionsPerHost = ConnectionsPerHost.Type
object ConnectionsPerHost extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input > 0 then true
    else s"connections per host must be a positive int, got $input"

  extension (value: Type) def toInt: Int = value.unwrap

/** Idle sockets kept for one host. `0` keeps none. */
type IdlePerHost = IdlePerHost.Type
object IdlePerHost extends Newtype[Int]:
  override inline def validate(input: Int): Boolean | String =
    if input >= 0 then true
    else s"idle connections per host must be >= 0, got $input"

  extension (value: Type) def toInt: Int = value.unwrap
