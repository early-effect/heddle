package heddle.endpoint

import heddle.error.{HeddleError, ParamError}

/** Why a success response did not decode into the endpoint's `Out`. */
enum OutputError(val message: String) extends HeddleError:
  case Body(error: BodyError)                  extends OutputError(error.message)
  case Header(name: String, error: ParamError) extends OutputError(s"header $name: ${error.message}")
