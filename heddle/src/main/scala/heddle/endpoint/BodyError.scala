package heddle.endpoint

import heddle.error.HeddleError
import heddle.http.Status

/** Why a body did not decode into the type its endpoint declares. */
enum BodyError(val message: String) extends HeddleError:
  case Unreadable(cause: Throwable) extends BodyError(s"the body could not be read: $cause")

  /** `detail` is the JSON decoder's own account of where the value stopped matching. */
  case Json(detail: String) extends BodyError(s"the body is not the declared JSON: $detail")

  /** An error body whose case answers with another status than the response carried. */
  case WrongStatus(declared: Status, actual: Status)
      extends BodyError(s"the error body answers with ${declared.code}, the response was ${actual.code}")
end BodyError
