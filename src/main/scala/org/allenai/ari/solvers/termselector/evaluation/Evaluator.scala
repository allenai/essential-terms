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
    *
    * @param testData TODO(daniel)
    * @param threshold TODO(daniel)
    * @param alpha TODO(daniel)
    * @return TODO(daniel)
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

  /** test per tokens on test data
    *
    * @param threshold TODO(daniel)
    * @param alpha TODO(daniel)
    * @return TODO(daniel)
    */
  def test(threshold: Double, alpha: Double): Map[String, (Double, Double, Double)] = {
    test(Sensors.testConstituents, threshold, alpha)
  }

  /** test per sentence on test data
    *
    * @param threshold TODO(daniel)
    * @param alpha TODO(daniel)
    * @return TODO(daniel)
    */
  def testAcrossSentences(threshold: Double, alpha: Double): Map[String, (Double, Double, Double)] = {
    testAcrossSentences(Sensors.testSentences, threshold, alpha)
  }

  /** test multiple sentences on test data
    *
    * @param sentences TODO(daniel)
    * @param threshold TODO(daniel)
    * @param alpha TODO(daniel)
    * @return TODO(daniel)
    */
  def testAcrossSentences(
    sentences: Iterable[Iterable[Constituent]], threshold: Double, alpha: Double
  ): Map[String, (Double, Double, Double)] = {
    val results = sentences.map(test(_, threshold, alpha))

    results.flatten.toList.groupBy({ tu: (String, _) => tu._1
    }).map {
      case (label, l) =>
        (label, MathUtils.avgTuple(l.map(_._2).reduce(MathUtils.sumTuple), l.length))
    }
  }

  def hammingMeasure(threshold: Double): Double = {
    val goldLabel = learner.dataModel.goldLabel
    val testerExact = new TestDiscrete
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](Sensors.testSentences)

    val hammingDistances = testReader.data.map { consIt =>
      consIt.map(cons => if (goldLabel(cons) != learner.predictLabel(cons, threshold)) 1 else 0).sum
    }
    logger.info("Average hamming distance = " + hammingDistances.sum.toDouble / hammingDistances.size)

    hammingDistances.sum.toDouble / hammingDistances.size
  }

  def printHammingDistances(threshold: Double): Unit = {
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](Sensors.testSentences)
    testReader.data.slice(0, 30).foreach { consIt =>
      val numSen = consIt.head.getTextAnnotation.getNumberOfSentences
      (0 until numSen).foreach(id =>
        logger.info(consIt.head.getTextAnnotation.getSentence(id).toString))

      val goldImportantSentence = consIt.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consIt.map(cons => convertToZeroOne(goldLabel(cons))).toSeq
      val predicted = consIt.map(cons => convertToZeroOne(learner.predictLabel(cons, threshold))).toSeq
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size.toDouble / predicted.size
      logger.info(goldImportantSentence)
      logger.info(goldStr)
      logger.info(predictedStr)
      logger.info(s"hamming distance = $hammingDistance")
      logger.info("----")
    }
  }

  def accuracyPerSentence(threshold: Double): Unit = {
    // harsh exact evaluation
    val testerExact = new TestDiscrete
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](Sensors.testSentences)
    testReader.data.foreach { consIt =>
      val gold = consIt.map(goldLabel(_)).mkString
      val predicted = consIt.map(learner.predictLabel(_, threshold)).mkString

      val fakePred = if (gold == predicted) "same" else "different"
      testerExact.reportPrediction(fakePred, "same")
    }
    testerExact.printPerformance(System.out)
  }

  def rankingMeasures(): Unit = {
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](Sensors.testSentences)
    testReader.reset()

    // ranking-based measures
    val averagePrecisionList = testReader.data.map { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(Constants.VIEW_NAME)
        .getConstituents.asScala
      val goldLabelList = consIt.toList.map { cons =>
        if (goldLabel(cons) == Constants.IMPORTANT_LABEL) 1 else 0
      }
      if (goldLabelList.sum <= 0) {
        val questionStr = consIt.head.getTextAnnotation.sentences().asScala.mkString("*")
        logger.warn(s"no essential term found in the gold annotation for question: $questionStr")
        0.5
      } else {
        val scoreLabelPairs = consIt.toList.map { cons =>
          val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
          val predScore = learner.predictProbOfBeingEssential(cons)
          (predScore, goldBinaryLabel)
        }
        val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
        meanAverageRankOfPositive(rankedGold)
      }
    }
    logger.info(s"Average ranked precision: ${averagePrecisionList.sum / averagePrecisionList.size}")

    // evaluating PR-curve over all tokens
    val scoreLabelPairs = testReader.data.flatMap { consIt =>
      consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = learner.predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
    }.toList
    val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
    val (precision, recall, _) = rankedPrecisionRecallYield(rankedGold).unzip3
    val writer = new PrintWriter(new File(learner.getSimpleName + learner.modelSuffix +
      "_rankingFeatures.txt"))
    writer.write(precision.mkString("\t") + "\n")
    writer.write(recall.mkString("\t") + "\n")
    writer.close()
    //Highcharts.areaspline(recall, precision)

    // per sentence
    val (perSenPList, perSenRList, perSenYList) = testReader.data.map { consIt =>
      val scoreLabelPairs = consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = learner.predictProbOfBeingEssential(cons)
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

    logger.info("Per sentence: ")
    logger.info(averageRList.mkString(", "))
    logger.info(averagePList.mkString(", "))
    logger.info(averageYList.mkString(", "))
    //    Highcharts.areaspline(averageRList, averagePList)
    //    Highcharts.xAxis("Recall")
    //    Highcharts.yAxis("Precision")
    //    Thread.sleep(10000L)
    //    Highcharts.stopServer

    // mean average precision/recall
    val avgPList = testReader.data.map { consIt =>
      val scoreLabelPairs = consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = learner.predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
      val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
      meanAverageRankOfPositive(rankedGold)
    }
    val map = avgPList.sum / avgPList.size
    logger.info(s"Mean Average Precision: $map")
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
    if (label == Constants.IMPORTANT_LABEL) 1 else 0
  }

  // averaging two lists of potentially different length
  private def avgList(list1: Seq[Double], list2: Seq[Double]): Seq[Double] = {
    val (shortList, longList) = if (list1.length < list2.length) (list1, list2) else (list2, list1)
    shortList.zipWithIndex.map { case (num, idx) => (longList(idx) + num) / 2 } ++
      longList.drop(shortList.length)
  }

  def printMistakes(threshold: Double): Unit = {
    learner.dataModel.essentialTermTokens.populate(Sensors.testConstituents, train = false)
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](Sensors.testSentences)
    testReader.reset()

    testReader.data.foreach { consIt =>
      val consList = consIt.toList
      val numSen = consList.head.getTextAnnotation.getNumberOfSentences
      (0 until numSen).foreach(id =>
        logger.info(consList.head.getTextAnnotation.getSentence(id).toString))
      val goldImportantSentence = consList.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consList.map(cons => convertToZeroOne(goldLabel(cons)))
      val predicted = consList.map(cons => convertToZeroOne(learner.predictLabel(cons, threshold)))
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size.toDouble / predicted.size
      logger.info(goldImportantSentence)
      logger.info(goldStr)
      logger.info(predictedStr)
      logger.info("Mistakes: ")
      consIt.toList.foreach { cons =>
        if (learner.predictLabel(cons, threshold) != goldLabel(cons)) {
          logger.info(cons.toString)
          logger.info(learner.combinedProperties(cons).toString())
          logger.info("correct label: " + goldLabel(cons))
          logger.info("-------")
        }
      }
      logger.info("=====")
    }
  }
}
