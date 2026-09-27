package heddle.http

import heddle.error.HeddleError

/** Why a `multipart/form-data` body could not be read. */
enum MultipartError(val message: String) extends HeddleError:
  case Undeclared extends MultipartError("the Content-Type names no multipart boundary")
  case NoBoundary extends MultipartError("the body holds no multipart boundary")
  case Truncated  extends MultipartError("the multipart body ends before its closing boundary")
