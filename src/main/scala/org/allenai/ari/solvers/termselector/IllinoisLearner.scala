package org.allenai.ari.solvers.termselector

import org.allenai.common.Logging
import org.allenai.ari.models.Question

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.lbjava.learn.StochasticGradientDescent
import edu.illinois.cs.cogcomp.saul.classifier.Learnable
import edu.illinois.cs.cogcomp.saul.parser.LBJIteratorParserScala

import com.quantifind.charts.Highcharts

import scala.collection.JavaConverters._

import java.io.{ File, PrintWriter }

object ai2Logger extends Logging {
  def trace(message: => String): Unit =
    if (internalLogger.isTraceEnabled) {
      internalLogger.trace(message)
    }

  def debug(message: => String): Unit =
    if (internalLogger.isDebugEnabled) {
      internalLogger.debug(message)
    }

  def info(message: => String): Unit =
    if (internalLogger.isInfoEnabled) {
      internalLogger.info(message)
    }

  def warn(message: => String): Unit =
    if (internalLogger.isWarnEnabled) {
      internalLogger.warn(message)
    }

  def warn(message: => String, throwable: Throwable): Unit =
    if (internalLogger.isWarnEnabled) {
      internalLogger.warn(message, throwable)
    }

  def error(message: => String): Unit =
    if (internalLogger.isErrorEnabled) {
      internalLogger.error(message)
    }

  def error(message: => String, throwable: Throwable): Unit =
    if (internalLogger.isErrorEnabled) {
      internalLogger.error(message, throwable)
    }
}

/** A parameterized abstract class for UIUC learners for essential terms detection. */
abstract class IllinoisLearner(
    essentialTermsDataModel: IllinoisDataModel
) extends Learnable[Constituent](essentialTermsDataModel.essentialTermTokens) with EssentialTermsLearner {

  /** This allows access to sub-classes of EssentialTermsDataModel if set appropriately by
    * inheriting classes.
    */
  def dataModel: IllinoisDataModel

  // implement for trait MyLearner
  def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    EssentialTermsSensors.getEssentialTermProbForAristoQuestion(aristoQuestion, this)
  }

  // implement for trait MyLearner
  def getEssentialTerms(aristoQuestion: Question, threshold: Double): Seq[String] = {
    EssentialTermsSensors.getEssentialTermsForAristoQuestion(aristoQuestion, this, threshold)
  }

  /** Predict the class label of a given term. */
  def predictLabel(c: Constituent, threshold: Double): String = {
    if (predictProbOfBeingEssential(c) > threshold) {
      EssentialTermsConstants.IMPORTANT_LABEL
    } else {
      EssentialTermsConstants.UNIMPORTANT_LABEL
    }
  }

  /** Predict whether a given term is essential. */
  def predictIsEssential(c: Constituent, threshold: Double): Boolean = {
    predictLabel(c, threshold) == EssentialTermsConstants.IMPORTANT_LABEL
  }

  private def convertRealsToProbabilities(realScore: Double): Double = 1 / (1 + Math.exp(-realScore))

  /** Predict the probability of a given term being essential. */
  def predictProbOfBeingEssential(c: Constituent): Double = {
    val rawScore = if (classifier.isInstanceOf[StochasticGradientDescent]) {
      classifier.realValue(c)
    } else {
      val scores = classifier.scores(c).toArray
      scores.find {
        case score => score.value == EssentialTermsConstants.IMPORTANT_LABEL
      }.get.score
    }
    convertRealsToProbabilities(rawScore)
  }

  /** test per tokens, given some data */
  def test(testData: Iterable[Constituent], threshold: Double, alpha: Double): Map[String, (Double, Double, Double)] = {
    val tester = new TestDiscrete
    testData.foreach { c =>
      tester.reportPrediction(predictLabel(c, threshold), dataModel.goldLabel(c))
    }
    tester.getLabels.map { label =>
      val F = if (!tester.getF(alpha, label).isNaN) tester.getF(alpha, label) else 0.0
      val P = if (!tester.getPrecision(label).isNaN) tester.getPrecision(label) else 0.0
      val R = if (!tester.getRecall(label).isNaN) tester.getRecall(label) else 0.0
      (label, (F, P, R))
    }.toMap
  }

  /* test per tokens on test data */
  def test(threshold: Double, alpha: Double): Map[String, (Double, Double, Double)] = {
    test(EssentialTermsSensors.testConstituents, threshold, alpha)
  }

  def testAcrossSentences(threshold: Double, alpha: Double): Map[String, (Double, Double, Double)] = {
    testAcrossSentences(EssentialTermsSensors.testSentences, threshold, alpha)
  }

  def testAcrossSentences(sentences: Iterable[Iterable[Constituent]], threshold: Double, alpha: Double): Map[String, (Double, Double, Double)] = {
    val results = sentences.map { sentenceCons =>
      test(sentenceCons, threshold, alpha)
    }

    results.flatten.toList.groupBy({
      tu: (String, (Double, Double, Double)) => tu._1
    }).map {
      case (label, l) =>
        val t = l.length
        val avg = avgTuple(l.map(_._2).reduce(sumTuple), t)
        (label, avg)
    }
  }

  private def sumTuple(
    a: (Double, Double, Double),
    b: (Double, Double, Double)
  ): (Double, Double, Double) = {
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  }

  private def avgTuple(a: (Double, Double, Double), size: Int): (Double, Double, Double) = {
    (a._1 / size, a._2 / size, a._3 / size)
  }

  def hammingMeasure(threshold: Double): Double = {
    val goldLabel = dataModel.goldLabel
    val testerExact = new TestDiscrete
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.testSentences)

    val hammingDistances = testReader.data.map { consIt =>
      consIt.map(cons => if (goldLabel(cons) != predictLabel(cons, threshold)) 1 else 0).sum
    }
    ai2Logger.info("Average hamming distance = " + hammingDistances.sum.toDouble / hammingDistances.size)

    hammingDistances.sum.toDouble / hammingDistances.size
  }

  def printHammingDistances(threshold: Double): Unit = {
    val goldLabel = dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.testSentences)
    testReader.data.slice(0, 30).foreach { consIt =>
      val numSen = consIt.head.getTextAnnotation.getNumberOfSentences
      (0 until numSen).foreach(id =>
        ai2Logger.info(consIt.head.getTextAnnotation.getSentence(id).toString))

      val goldImportantSentence = consIt.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consIt.map(cons => convertToZeroOne(goldLabel(cons))).toSeq
      val predicted = consIt.map(cons => convertToZeroOne(predictLabel(cons, threshold))).toSeq
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size.toDouble / predicted.size
      ai2Logger.info(goldImportantSentence)
      ai2Logger.info(goldStr)
      ai2Logger.info(predictedStr)
      ai2Logger.info(s"hamming distance = $hammingDistance")
      ai2Logger.info("----")
    }
  }

  def accuracyPerSentence(threshold: Double): Unit = {
    // harsh exact evaluation
    val testerExact = new TestDiscrete
    val goldLabel = dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.testSentences)
    testReader.data.foreach { consIt =>
      val gold = consIt.map(goldLabel(_)).mkString
      val predicted = consIt.map(predictLabel(_, threshold)).mkString

      val fakePred = if (gold == predicted) "same" else "different"
      testerExact.reportPrediction(fakePred, "same")
    }
    testerExact.printPerformance(System.out)
  }

  def rankingMeasures(): Unit = {
    val goldLabel = dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.testSentences)
    testReader.reset()

    // ranking-based measures
    val averagePrecisionList = testReader.data.map { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME)
        .getConstituents.asScala
      val goldLabelList = consIt.toList.map { cons =>
        if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
      }
      if (goldLabelList.sum <= 0) {
        ai2Logger.warn("no essential term in gold found in the gold annotation of this question .... ")
        ai2Logger.warn(s"question: ${consIt.head.getTextAnnotation.sentences().asScala.mkString("*")}")
        0.5
      } else {
        val scoreLabelPairs = consIt.toList.map { cons =>
          val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
          val predScore = predictProbOfBeingEssential(cons)
          (predScore, goldBinaryLabel)
        }
        val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
        meanAverageRankOfPositive(rankedGold)
      }
    }
    ai2Logger.info(s"Average ranked precision: ${averagePrecisionList.sum / averagePrecisionList.size}")

    // evaluating PR-curve over all tokens
    val scoreLabelPairs = testReader.data.flatMap { consIt =>
      consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
    }.toList
    val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
    val (precision, recall, _) = rankedPrecisionRecallYield(rankedGold).unzip3
    val writer = new PrintWriter(new File(this.getSimpleName + this.modelSuffix + "_rankingFeatures.txt"))
    writer.write(precision.mkString("\t") + "\n")
    writer.write(recall.mkString("\t") + "\n")
    writer.close()
    //Highcharts.areaspline(recall, precision)

    // per sentence
    val (perSenPList, perSenRList, perSenYList) = testReader.data.map { consIt =>
      val scoreLabelPairs = consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
      val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
      val (precision, recall, yyield) = rankedPrecisionRecallYield(rankedGold).unzip3
      (precision, recall, yyield)
    }.unzip3

    val averagePList = perSenPList.reduceRight[Seq[Double]] { case (a, b) => avgList(a, b) }
    val averageRList = perSenRList.reduceRight[Seq[Double]] { case (a, b) => avgList(a, b) }
    val averageYList = perSenYList.reduceRight[Seq[Double]] { case (a, b) => avgList(a, b) }
    assert(averagePList.length == averageRList.length)
    assert(averagePList.length == averageYList.length)

    ai2Logger.info("Per sentence: ")
    ai2Logger.info(averageRList.mkString(", "))
    ai2Logger.info(averagePList.mkString(", "))
    ai2Logger.info(averageYList.mkString(", "))
    //    Highcharts.areaspline(averageRList, averagePList)
    //    Highcharts.xAxis("Recall")
    //    Highcharts.yAxis("Precision")
    //    Thread.sleep(10000L)
    //    Highcharts.stopServer

    // mean average precision/recall
    val avgPList = testReader.data.map { consIt =>
      val scoreLabelPairs = consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
      val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
      meanAverageRankOfPositive(rankedGold)
    }
    val map = avgPList.sum / avgPList.size
    ai2Logger.info(s"Mean Average Precision: ${map}")
  }

  /** gold is a vector of 1/0, where the elements are sorted according to their prediction scores
    * The higher the score is, the earlier the element shows up in the gold list
    */
  def meanAverageRankOfPositive(gold: Seq[Int]): Double = {
    require(gold.sum > 0, "There is no essential term in this sentence! ")
    val totalPrecisionScore = gold.zipWithIndex.collect {
      case (1, idx) => gold.slice(0, idx + 1).sum.toDouble / (idx + 1)
    }
    totalPrecisionScore.sum / totalPrecisionScore.size
  }

  // gold is a Seq of 0 and 1
  def rankedPrecisionRecallYield(gold: Seq[Int]): Seq[(Double, Double, Double)] = {
    val sum = gold.sum
    val totalPrecisionScore = gold.zipWithIndex.map {
      case (g, idx) =>
        val sumSlice = gold.slice(0, idx + 1).sum.toDouble
        val precision = sumSlice / (1 + idx)
        val recall = sumSlice / sum
        val yyield = idx.toDouble
        (precision, recall, yyield)
    }
    totalPrecisionScore
  }

  private def convertToZeroOne(label: String): Int = {
    if (label == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
  }

  private def avg(list: List[Double]): Double = {
    list.sum / list.size
  }

  // averaging two lists of potentially different length
  private def avgList(list1: Seq[Double], list2: Seq[Double]): Seq[Double] = {
    val (shortList, longList) = if (list1.length < list2.length) (list1, list2) else (list2, list1)
    shortList.zipWithIndex.map { case (num, idx) => (longList(idx) + num) / 2 } ++
      longList.drop(shortList.length)
  }

  def printMistakes(threshold: Double): Unit = {
    dataModel.essentialTermTokens.populate(EssentialTermsSensors.testConstituents, train = false)
    val goldLabel = dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.testSentences)
    testReader.reset()

    testReader.data.foreach { consIt =>
      val consList = consIt.toList
      val numSen = consList.head.getTextAnnotation.getNumberOfSentences
      (0 until numSen).foreach(id =>
        ai2Logger.info(consList.head.getTextAnnotation.getSentence(id).toString))
      val goldImportantSentence = consList.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consList.map(cons => convertToZeroOne(goldLabel(cons)))
      val predicted = consList.map(cons => convertToZeroOne(predictLabel(cons, threshold)))
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size.toDouble / predicted.size
      ai2Logger.info(goldImportantSentence)
      ai2Logger.info(goldStr)
      ai2Logger.info(predictedStr)
      ai2Logger.info("Mistakes: ")
      consIt.toList.foreach { cons =>
        if (predictLabel(cons, threshold) != goldLabel(cons)) {
          ai2Logger.info(cons.toString)
          ai2Logger.info(combinedProperties(cons).toString())
          ai2Logger.info("correct label: " + goldLabel(cons))
          ai2Logger.info("-------")
        }
      }
      ai2Logger.info("=====")
    }
  }

  /** given a set of training instances it returns the optimal threshold */
  def tuneThreshold(alpha: Double): Double = {
    val goldLabel = dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](EssentialTermsSensors.devSentences)
    testReader.reset()

    // first get the real predictions per sentence
    val scoreLabelPairs = testReader.data.map { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME)
        .getConstituents.asScala
      val goldLabelList = consIt.toList.map { cons =>
        if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
      }
      consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
    }

    // tune the threshold
    val initialThreshold = 0.5
    val totalIterations = 20
    def stepSize(k: Int): Double = 0.2 / (k + 1)
    val (minTh, maxTh) = (scoreLabelPairs.map { a => a.unzip._1.min }.min, scoreLabelPairs.map { a => a.unzip._1.max }.max)
    val thresholdRange = minTh to maxTh by 0.01

    // recursive
    def singleIteration(currentThreshold: Double, remainingIterations: Int, currentScore: Double): (Double, Double) = {
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
        singleIteration(topTh, remainingIterations - 1, topF)
      }
    }

    // try all points
    def tryGridOfThresholds(): (Double, (Double, Double, Double)) = {
      val scores = thresholdRange.map { th => th -> evaluateFAllSentences(scoreLabelPairs, th, alpha) }
      //      println("all the scores " + scores)
      scores.sortBy { _._2._1 }.last
    }

    def evaluateFAllSentences(scoreLabelPairs: Iterable[Seq[(Double, Int)]], threshold: Double, alpha: Double): (Double, Double, Double) = {
      val (fList, pList, rList) = scoreLabelPairs.map { scoreLabelPairsPerSentence =>
        evaluateFSingleSentence(scoreLabelPairsPerSentence, threshold, alpha)
      }.toList.unzip3
      (fList.sum / fList.length, pList.sum / pList.length, rList.sum / rList.length)
    }

    def evaluateFSingleSentence(scoreLabelPairs: Seq[(Double, Int)], threshold: Double, alpha: Double): (Double, Double, Double) = {
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

    val (topThreshold, topScore) = tryGridOfThresholds() // singleIteration(initialThreshold, totalIterations, -1)
    ai2Logger.info(s"Score after tuning: $topScore / threshold after tuning: $topThreshold / alpha: $alpha")

    topThreshold
  }
}
