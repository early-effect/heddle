package heddle.client

import heddle.http.Status

/** Why a typed call to an `Endpoint[In, E, Out]` did not produce an `Out`. */
enum CallFailure[+E]:
  /** The endpoint answered with one of its own declared errors. */
  case Domain(error: E)

  /** No response arrived. */
  case Transport(error: ClientError)

  /** A declared status arrived with a body the endpoint's codecs reject. */
  case Undecodable(status: Status, reason: String)

  /** A status the endpoint does not declare. */
  case Unexpected(status: Status)
end CallFailure
