package heddle.endpoint

import heddle.http.Status

/** The status one case `A` of an endpoint's error ADT answers with. Listed inline in `Endpoint.outErrors`. */
final case class ErrorCase[A](status: Status)
