package heddle.internal.engine

/** Socket failures the keep-alive loop treats as a deadline or an interrupt, matched on the class so a message cannot
  * change the branch.
  */
private[heddle] object IoKind:
  def closedByInterrupt(e: Throwable): Boolean =
    e.getClass.getName.endsWith("ClosedByInterruptException")

  def socketTimeout(e: Throwable): Boolean =
    e.getClass.getName.endsWith("SocketTimeoutException")
end IoKind
