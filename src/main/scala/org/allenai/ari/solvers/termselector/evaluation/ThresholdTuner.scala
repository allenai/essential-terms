package org.allenai.ari.solvers.termselector.evaluation

import org.allenai.ari.solvers.termselector.learners.IllinoisLearner
import org.allenai.ari.solvers.termselector.{ Constants, Sensors }
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.saul.parser.IterableToLBJavaParser

import scala.collection.JavaConverters._
import scala.collection.immutable.NumericRange

/** Threshold tuner for a learner. */
class ThresholdTuner(learner: IllinoisLearner) extends Logging {
  private val initialThreshold = 0.5
  private val totalIterations = 20

  /** given a set of training instances it returns the optimal threshold (i.e. maximize of F_alpha) */
  def tuneThreshold(alpha: Double): Double = {
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](Sensors.devSentences)
    testReader.reset()

    // first get the real predictions per sentence
    val scoreLabelPairs: Iterable[List[(Double, Int)]] = testReader.data.map { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(Constants.VIEW_NAME).getConstituents.asScala
      val goldLabelList = consIt.toList.map { cons =>
        if (goldLabel(cons) == Constants.IMPORTANT_LABEL) 1 else 0
      }
      consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = learner.predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
    }

    val minTh = scoreLabelPairs.map { a => a.unzip._1.min }.min
    val maxTh = scoreLabelPairs.map { a => a.unzip._1.max }.max
    val thresholdRange = minTh to maxTh by 0.01

    val (topThreshold, topScore) = tryGridOfThresholds(scoreLabelPairs, alpha, thresholdRange)
    logger.info(s"Score after tuning: $topScore / threshold: $topThreshold / alpha: $alpha")
    topThreshold
  }

  /** Evaluates triple of (F_alpha, Precision and recall) for each value of threshold defined by its range
    * @param scoreLabelPairs the predictions on the sentences
    * @param alpha used in evaluation of F_alpha
    * @param thresholdRange a range of thresholds to evaluate on
    * @return triple of F_alpha, Precision and recall, per choice of threshold
    */
  private def tryGridOfThresholds(
    scoreLabelPairs: Iterable[List[(Double, Int)]],
    alpha: Double,
    thresholdRange: NumericRange[Double]
  ): (Double, (Double, Double, Double)) = {
    val scores = thresholdRange.map { th => th -> evaluateFAllSentences(scoreLabelPairs, th, alpha) }
    scores.sortBy { _._2._1 }.last
  }

  /** Calculates average F_alpha, precision, recall across sentences
    * @param scoreLabelPairs predictions of the sentence
    * @param threshold used for creating binary predictions; constituents with score above this are essential
    * @param alpha used in evaluation of F_alpha
    * @return a triple of F_alpha, Precision and recall.
    */
  private def evaluateFAllSentences(
    scoreLabelPairs: Iterable[Seq[(Double, Int)]],
    threshold: Double, alpha: Double
  ): (Double, Double, Double) = {
    val (fList, pList, rList) = scoreLabelPairs.map { scoreLabelPairsPerSentence =>
      evaluateFSingleSentence(scoreLabelPairsPerSentence, threshold, alpha)
    }.toList.unzip3
    (fList.sum / fList.length, pList.sum / pList.length, rList.sum / rList.length)
  }

  /** For a given sentence, it evaluates F_alpha, precision, recall
    * @param scoreLabelPairs predictions of the sentence
    * @param threshold used for creating binary predictions; constituents with score above this are essential
    * @param alpha used in evaluation of F_alpha
    * @return a triple of F_alpha, Precision and recall.
    */
  private def evaluateFSingleSentence(scoreLabelPairs: Seq[(Double, Int)], threshold: Double,
    alpha: Double): (Double, Double, Double) = {
    val testDiscrete = new TestDiscrete
    scoreLabelPairs.foreach {
      case (score, label) =>
        testDiscrete.reportPrediction(if (score > threshold) "1" else "0", label.toString)
    }
    val f = testDiscrete.getF(alpha, "1")
    val p = testDiscrete.getPrecision("1")
    val r = testDiscrete.getRecall("1")
    (if (f.isNaN) 0 else f, if (p.isNaN) 0 else p, if (r.isNaN) 0 else r)
  }

  private def convertToZeroOne(label: String): Int = {
    if (label == Constants.IMPORTANT_LABEL) 1 else 0
  }
}
