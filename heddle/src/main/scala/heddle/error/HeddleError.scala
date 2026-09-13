package heddle.error

trait HeddleError:
  def message: String
  override def toString: String = message
