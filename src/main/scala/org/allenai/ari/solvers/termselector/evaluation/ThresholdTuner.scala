package org.allenai.ari.solvers.termselector.evaluation

import org.allenai.ari.solvers.termselector.learners.IllinoisLearner
import org.allenai.ari.solvers.termselector.{ Constants, EssentialTermsSensors }

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.saul.parser.LBJIteratorParserScala

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.NumericRange

/** Threshold tuner for a learner. */
class ThresholdTuner(learner: IllinoisLearner) {

  private val initialThreshold = 0.5
  private val totalIterations = 20
  private def stepSize(k: Int): Double = 0.2 / (k + 1)

  /** given a set of training instances it returns the optimal threshold */
  def tuneThreshold(alpha: Double): Double = {
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.devSentences)
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
    //    val (topThreshold, topScore) = singleIteration(scoreLabelPairs, alpha, initialThreshold,
    //      totalIterations, -1)
    ai2Logger.info(s"Score after tuning: $topScore / threshold: $topThreshold / alpha: $alpha")

    topThreshold
  }

  // try all points
  def tryGridOfThresholds(
    scoreLabelPairs: Iterable[List[(Double, Int)]],
    alpha: Double,
    thresholdRange: NumericRange[Double]
  ): (Double, (Double, Double, Double)) = {
    val scores = thresholdRange.map { th => th -> evaluateFAllSentences(scoreLabelPairs, th, alpha) }
    //      println("all the scores " + scores)
    scores.sortBy { _._2._1 }.last
  }

  // recursive
  @tailrec
  private def singleIteration(
    scoreLabelPairs: Iterable[List[(Double, Int)]],
    alpha: Double,
    currentThreshold: Double,
    remainingIterations: Int,
    currentScore: Double
  ): (Double, Double) = {
    if (remainingIterations == 0) {
      (currentThreshold, currentScore)
    } else {
      val thresholdUp = currentThreshold + stepSize(totalIterations - remainingIterations)
      val thresholdDown = currentThreshold - stepSize(totalIterations - remainingIterations)
      val FUp = evaluateFAllSentences(scoreLabelPairs, thresholdUp, alpha)._1
      val FDown = evaluateFAllSentences(scoreLabelPairs, thresholdDown, alpha)._1
      val thresholdScoresPairs = List((thresholdUp, FUp), (thresholdDown, FDown), (currentThreshold, currentScore))
      val (topTh, topF) = thresholdScoresPairs.sortBy(_._2).last
      ai2Logger.debug(s"Iteration:$remainingIterations / score: $currentScore / threshold: $topTh")
      singleIteration(scoreLabelPairs, alpha, topTh, remainingIterations - 1, topF)
    }
  }

  private def evaluateFAllSentences(
    scoreLabelPairs: Iterable[Seq[(Double, Int)]],
    threshold: Double, alpha: Double
  ): (Double, Double, Double) = {
    val (fList, pList, rList) = scoreLabelPairs.map { scoreLabelPairsPerSentence =>
      evaluateFSingleSentence(scoreLabelPairsPerSentence, threshold, alpha)
    }.toList.unzip3
    (fList.sum / fList.length, pList.sum / pList.length, rList.sum / rList.length)
  }

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
