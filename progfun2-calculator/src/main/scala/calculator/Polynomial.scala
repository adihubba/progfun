package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4.0 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      val d = delta()
      if (d < 0.0) {
        Set()
      } else {
        val deltaSqrt = math.sqrt(delta())
        Set((-b() + deltaSqrt) / (2.0 * a()),
            (-b() - deltaSqrt) / (2.0 * a()))
      }
    })
  }
}
