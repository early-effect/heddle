package heddle.error

/** Why a query value or header could not become the type its codec reads. */
enum ParamError(val message: String) extends HeddleError:
  case Missing                                  extends ParamError("missing")
  case Malformed(raw: String, expected: String) extends ParamError(s"expected $expected, got '$raw'")
