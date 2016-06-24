package org.allenai.ari.solvers.termselector.evaluation

/** A few helpful math utility methods. */
object MathUtils {

  def sumTuple(
    a: (Double, Double, Double),
    b: (Double, Double, Double)
  ): (Double, Double, Double) = {
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  }

  def avgTuple(a: (Double, Double, Double), size: Int): (Double, Double, Double) = {
    require(size > 0, "size must be positive")
    (a._1 / size, a._2 / size, a._3 / size)
  }
}
