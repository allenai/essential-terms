package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.{ ParentheticalChoiceIdentifier, Question }
import org.allenai.ari.solvers.termselector.EssentialTermsSensors._
import org.allenai.common.Logging

import com.quantifind.charts.Highcharts
import com.redis._
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.saul.parser.LBJIteratorParserScala
import spray.json._
import DefaultJsonProtocol._

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

import java.io.{ File, PrintWriter }

/** A sample application to train, test, save, and load essential terms classifiers. */
class EssentialTermsApp(loadSavedModel: Boolean) extends Logging {
  // lazily create the baseline and expanded data models and learners
  private lazy val (baselineDataModel, baselineLearners, expandedDataModel, expandedLearner) = {
    ExpandedLearner.makeNewLearner(loadSavedModel)
  }

  def trainAndTestBaselineLearners(testOnSentences: Boolean = false): Unit = {
    trainAndTestLearner(baselineLearners.surfaceForm, 1, test = true, testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.lemma, 1, test = true, testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.posConjLemma, 1, test = true, testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.wordFormConjNer, 1, test = true, testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.wordFormConjNerConjPos, 1, test = true, testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.baselineLearnerLemmaPair, 1, test = true, testOnSentences, saveModel = true)
  }

  def trainAndTestExpandedLearner(testOnSentences: Boolean = false): Unit = {
    // since baselineLearner is used in expandedLearner, first train the baseline
    trainAndTestBaselineLearners(testOnSentences)
    trainAndTestLearner(expandedLearner, 20, test = true, testOnSentences, saveModel = true)
  }

  def loadAndTestExpandedLearner(): Unit = {
    testLearner(baselineLearners.surfaceForm, test = true, testOnSentences = false)
    testLearner(baselineLearners.lemma, test = true, testOnSentences = false)
    testLearner(baselineLearners.posConjLemma, test = true, testOnSentences = false)
    testLearner(baselineLearners.wordFormConjNer, test = true, testOnSentences = false)
    testLearner(baselineLearners.wordFormConjNerConjPos, test = true, testOnSentences = false)
    testLearner(expandedLearner, test = true, testOnSentences = true)
  }

  def testLearnerWithSampleAristoQuestion(): Unit = {
    val q = "In New York State, the longest period of daylight occurs during which month? (A) December (B) June (C) March (D) September"
    //    val q = " What force causes a feather to fall slower than a rock? " +
    //      "(A) gravity (B) air resistance (C) magnetism (D) electricity"
    val maybeSplitQuestion = ParentheticalChoiceIdentifier(q)
    val multipleChoiceSelection = EssentialTermsUtils.fallbackDecomposer(maybeSplitQuestion)
    val aristoQuestion = Question(q, Some(maybeSplitQuestion.question), multipleChoiceSelection)
    val essentialTerms = getEssentialTermsForAristoQuestion(aristoQuestion, expandedLearner)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
    logger.info(expandedLearner.getEssentialTermScores(aristoQuestion).toString)
  }

  def cacheSalienceScoresInRedis(): Unit = {
    val r = new RedisClient("localhost", 6379)
    allQuestions.foreach { q =>
      if (!r.exists(q.rawQuestion) && q.aristoQuestion.selections.nonEmpty) {
        logger.debug(" ===> Caching . . . ")
        logger.debug(q.rawQuestion)
        logger.debug(q.aristoQuestion.toString)
        val resultFuture = salienceScorer.salienceFor(q.aristoQuestion)
        val result = Await.result(resultFuture, Duration.Inf)
        val resultJson = result.toList.toJson
        r.set(q.rawQuestion, resultJson.compactPrint)
      } else {
        logger.debug(" ===> Skipping . . . ")
        logger.debug(q.rawQuestion)
        logger.debug(q.aristoQuestion.toString)
      }
    }
    actorSystem.terminate()
  }

  def saveSalienceCacheOnDisk(): Unit = {
    val r = new RedisClient("localhost", 6379)
    val salienceCacheFile = "salienceCache.txt"
    val writer = new PrintWriter(new File(salienceCacheFile))
    allQuestions.foreach { q =>
      if (r.exists(q.rawQuestion) && q.aristoQuestion.selections.nonEmpty) {
        writer.write(s"${q.rawQuestion}\n${r.get(q.rawQuestion).get}\n")
      } else {
        logger.debug(" ===> Skipping . . . ")
      }
    }
    writer.close()
  }

  private def trainAndTestLearner(
    learner: IllinoisLearner,
    numIterations: Int,
    test: Boolean = true,
    testOnSentences: Boolean = false,
    saveModel: Boolean = false
  ): Unit = {
    val dataModel = learner.dataModel
    // load the data into the model
    dataModel.essentialTermTokens.clear
    dataModel.essentialTermTokens.populate(trainConstiuents)
    dataModel.essentialTermTokens.populate(testConstituents, train = false)

    // train
    logger.debug(s"Training learner ${learner.getSimpleName} for $numIterations iterations")
    learner.learn(numIterations)

    if (saveModel) {
      logger.debug(s"Saving model ${learner.getSimpleName} at ${learner.lcFilePath}")
      learner.save()
    }

    testLearner(learner, test, testOnSentences)
  }

  private def testLearner(
    learner: IllinoisLearner,
    test: Boolean,
    testOnSentences: Boolean
  ): Unit = {
    val dataModel = learner.dataModel
    // load the data into the model
    dataModel.essentialTermTokens.clear
    dataModel.essentialTermTokens.populate(testConstituents, train = false)

    // test
    if (test) {
      logger.debug(s"Testing learner ${learner.getSimpleName}")
      learner.test()
    }
    // microAvgTest(baselineLearner)
    if (testOnSentences) {
      logger.debug(s"Testing learner ${learner.getSimpleName} over sentences")
      testOverSentences(learner)
    }
  }

  private def microAvgTest(learner: IllinoisLearner): Unit = {
    logger.info("Micro-average = ")
    val results = testSentences.map { sentenceCons =>
      learner.test(sentenceCons)
    }
    logger.info(results.toString())

    results.flatten.toList.groupBy({
      tu: (String, (Double, Double, Double)) => tu._1
    }).foreach({
      case (label, l) =>
        val t = l.length
        val avg = avgTuple(l.map(_._2).reduce(sumTuple), t)
        printTestResult((label, avg))
    })
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

  private def printTestResult(result: (String, (Double, Double, Double))): Unit = {
    result match {
      case (label, (f1, precision, recall)) =>
        logger.info(s"  $label    $f1    $precision     $recall   ")
    }
  }

  private def convertToZeroOne(label: String): Integer = if (label == "IMPORTANT") 1 else 0

  private def testOverSentences(learner: IllinoisLearner): Unit = {
    val goldLabel = learner.dataModel.goldLabel
    val testerExact = new TestDiscrete
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](testSentences)
    testReader.reset()

    // ranking-based measures
    if (!learner.isInstanceOf[BaselineLearner]) {
      // for BaselineLearner, "predictProbOfBeingEssential" is not defined
      val averagePrecisionList = testReader.data.map { consIt =>
        val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala
        val goldLabelList = consIt.toList.map { cons =>
          if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
        }
        if (goldLabelList.sum <= 0) {
          logger.warn("no essential term in gold found in the gold annotation of this question .... ")
          logger.warn(s"question: ${consIt.head.getTextAnnotation.sentences().asScala.mkString("*")}")
          0.5
        } else {
          val scoreLabelPairs = consIt.toList.map { cons =>
            val goldBinaryLabel = if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
            val predScore = learner.predictProbOfBeingEssential(cons)
            (predScore, goldBinaryLabel)
          }
          val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
          meanAverageRank(rankedGold)
        }
      }
      logger.info(s"Average ranked precision: ${averagePrecisionList.sum / averagePrecisionList.size}")
    }

    val hammingDistances = testReader.data.map { consIt =>
      consIt.map(cons => if (goldLabel(cons) != learner.predictLabel(cons)) 1 else 0).sum
    }
    logger.info("Average hamming distance = " + hammingDistances.sum.toDouble / hammingDistances.size)

    testReader.data.slice(0, 30).foreach { consIt =>
      val numSen = consIt.head.getTextAnnotation.getNumberOfSentences
      (0 until numSen).foreach(id =>
        logger.info(consIt.head.getTextAnnotation.getSentence(id).toString))

      val goldImportantSentence = consIt.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consIt.map(cons => convertToZeroOne(goldLabel(cons))).toSeq
      val predicted = consIt.map(cons => convertToZeroOne(learner.predictLabel(cons))).toSeq
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size.toDouble / predicted.size
      logger.info(goldImportantSentence)
      logger.info(goldStr)
      logger.info(predictedStr)
      logger.info(s"hamming distance = $hammingDistance")
      logger.info("----")
    }

    // harsh exact evaluation
    testReader.data.foreach { consIt =>
      val gold = consIt.map(goldLabel(_)).mkString
      val predicted = consIt.map(learner(_)).mkString

      val fakePred = if (gold == predicted) "same" else "different"
      testerExact.reportPrediction(fakePred, "same")
    }
    testerExact.printPerformance(System.out)

    // precision recall curve
    // because for BaselineLearner, "predictProbOfBeingEssential" is not defined
    if (!learner.isInstanceOf[BaselineLearner]) {
      //       evaluating PR-curve over all tokens
      val scoreLabelPairs = testReader.data.flatMap { consIt =>
        consIt.toList.map { cons =>
          val goldBinaryLabel = if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
          val predScore = learner.predictProbOfBeingEssential(cons)
          (predScore, goldBinaryLabel)
        }
      }.toList
      val rankedGold = scoreLabelPairs.sortBy(-_._1).map(_._2)
      val (precision, recall, _) = rankedPrecisionRecallYield(rankedGold).unzip3
      Highcharts.areaspline(recall, precision)

      // per sentence
      val (perSenPList, perSenRList, perSenYList) = testReader.data.map { consIt =>
        val scoreLabelPairs = consIt.toList.map { cons =>
          val goldBinaryLabel = if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
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

      logger.info(averageRList.mkString("/"))
      logger.info(averagePList.mkString("/"))
      logger.info(averageYList.mkString("/"))
      //Highcharts.areaspline(averageRList, averagePList)

      Highcharts.xAxis("Recall")
      Highcharts.yAxis("Precision")
      Thread.sleep(30000L)
      Highcharts.stopServer
    }
  }

  private def avg(list: List[Double]): Double = {
    list.sum / list.size
  }

  // averaging two lists of potentially different length
  private def avgList(list1: Seq[Double], list2: Seq[Double]): Seq[Double] = {
    val (shortList, longList) = if (list1.length < list2.length) (list1, list2) else (list2, list1)
    shortList.zipWithIndex.map { case (num, idx) => (longList(idx) + num) / 2 } ++ longList.drop(shortList.length)
  }

  /** gold is a vector of 1/0, where the elements are sorted according to their prediction scores
    * The higher the score is, the earlier the element shows up in the gold list
    */
  def meanAverageRank(gold: Seq[Int]): Double = {
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

  private def printMistakes(): Unit = {
    val dataModel = expandedLearner.dataModel
    dataModel.essentialTermTokens.populate(testConstituents, train = false)
    val goldLabel = expandedLearner.dataModel.goldLabel
    val testerExact = new TestDiscrete
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](testSentences)
    testReader.reset()

    testReader.data.foreach { consIt =>
      val consList = consIt.toList
      val numSen = consList.head.getTextAnnotation.getNumberOfSentences
      if (internalLogger.isInfoEnabled()) {
        (0 until numSen).foreach(id =>
          logger.info(consList.head.getTextAnnotation.getSentence(id).toString))
      }
      val goldImportantSentence = consList.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consList.map(cons => convertToZeroOne(goldLabel(cons))).toSeq
      val predicted = consList.map(cons => convertToZeroOne(expandedLearner.predictLabel(cons))).toSeq
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size.toDouble / predicted.size
      logger.info(goldImportantSentence)
      logger.info(goldStr)
      logger.info(predictedStr)
      logger.info("Mistakes: ")
      consIt.toList.foreach { cons =>
        if (expandedLearner.predictLabel(cons) != goldLabel(cons)) {
          logger.info(cons.toString)
          logger.info(expandedLearner.combinedProperties(cons).toString())
          logger.info("correct label: " + goldLabel(cons))
          logger.info("-------")
        }
      }
      logger.info("=====")
    }
  }
}
/** An EssentialTermsApp companion object with main() method. */
object EssentialTermsApp extends Logging {
  def main(args: Array[String]): Unit = {
    val usageStr = "\nUSAGE: run 1 (TrainAndTestMainLearner) | 2 (LoadAndTestMainLearner) | " +
      "3 (TrainAndTestBaseline) | 4 (TestWithAristoQuestion) | 5 (CacheSalienceScores) |" +
      " 6 (PrintMistakes) "
    if (args.isEmpty || args.length > 1) {
      throw new IllegalArgumentException(usageStr)
    } else {
      val testType = args(0)
      testType match {
        case "1" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false)
          essentialTermsApp.trainAndTestExpandedLearner(testOnSentences = true)
        case "2" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true)
          essentialTermsApp.loadAndTestExpandedLearner()
        case "3" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false)
          essentialTermsApp.trainAndTestBaselineLearners(testOnSentences = false)
        case "4" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true)
          essentialTermsApp.testLearnerWithSampleAristoQuestion()
        case "5" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false)
          essentialTermsApp.cacheSalienceScoresInRedis()
        case "6" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true)
          essentialTermsApp.printMistakes()
        case _ =>
          throw new IllegalArgumentException(s"Unrecognized run option; $usageStr")
      }
    }
  }
}
