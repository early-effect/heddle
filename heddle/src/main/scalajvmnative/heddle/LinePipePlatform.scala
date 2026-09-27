package heddle

private[heddle] object LinePipePlatform:
  def standard: LinePipe = LinePipe.streams(java.lang.System.in, java.lang.System.out)
