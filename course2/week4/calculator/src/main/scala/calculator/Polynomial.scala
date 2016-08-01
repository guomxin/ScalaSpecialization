package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] =
    Signal {
      val bValue = b()
      bValue * bValue - 4 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val deltaValue = delta()
    if (deltaValue < 0) Set()
    else Set((-b() + math.sqrt(deltaValue)) / (2 * a()), (-b() - math.sqrt(deltaValue)) / (2 * a()))
  }
}
