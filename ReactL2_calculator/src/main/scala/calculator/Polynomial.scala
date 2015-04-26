package calculator


object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal {
      val _b = b()
      _b * _b - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {
      if (delta() < 0 | a() == 0) {
        Set.empty
      } else if (delta() == 0) {
        Set((-b() + Math.sqrt(delta())) /
          (2 * a())
        )
      } else {
        Set(
          (-b() + Math.sqrt(delta())) /
            (2 * a())
          ,
          (-b() - Math.sqrt(delta())) /
            (2 * a())
        )
      }
    }
  }
}
