package org.allenai.ari.solvers.termselector.evaluation

import org.allenai.ari.solvers.termselector.learners.IllinoisLearner
import org.allenai.ari.solvers.termselector.{ Constants, Sensors }
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.saul.parser.IterableToLBJavaParser

import scala.collection.JavaConverters._

import java.io.{ File, PrintWriter }

/** Various methods to evaluate the performance of an IllinoisLearner. */
class Evaluator(learner: IllinoisLearner) extends Logging {

  /** test per tokens, given some data
    * @param testData the input constituent
    * @param threshold constituents with score above this are essential
    * @param alpha the parameter for calculating F-measure
    * @return a map from output label (essential, or non-essential) to tuple of F-alpha, precision and recall.
    */
  def test(
    testData: Iterable[Constituent], threshold: Double, alpha: Double
  ): Map[String, (Double, Double, Double)] = {
    val tester = new TestDiscrete
    testData.foreach { c =>
      tester.reportPrediction(learner.predictLabel(c, threshold), learner.dataModel.goldLabel(c))
    }
    tester.getLabels.map { label =>
      val F = if (!tester.getF(alpha, label).isNaN) tester.getF(alpha, label) else 0d
      val P = if (!tester.getPrecision(label).isNaN) tester.getPrecision(label) else 0d
      val R = if (!tester.getRecall(label).isNaN) tester.getRecall(label) else 0d
      (label, (F, P, R))
    }.toMap
  }
}
