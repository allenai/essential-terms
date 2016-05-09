package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.{ ParentheticalChoiceIdentifier, Question }
import org.allenai.ari.solvers.termselector.EssentialTermsSensors._
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import edu.illinois.cs.cogcomp.saul.parser.LBJIteratorParserScala

import com.redis._
import spray.json._
import DefaultJsonProtocol._

import java.io.{ File, PrintWriter }

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

/** A sample application to train, test, save, and load essential terms classifiers. */
class EssentialTermsApp(loadSavedModel: Boolean) extends Logging {
  // lazily create the baseline and expanded data models and learners
  private lazy val (baselineDataModel, baselineLearner, expandedDataModel, expandedLearner) = {
    ExpandedLearner.makeNewLearner(loadSavedModel)
  }

  def trainAndTestBaselineLearner(testOnSentences: Boolean = false): Unit = {
    trainAndTestLearner(baselineLearner, 1, test = true, testOnSentences)
  }

  def trainAndTestExpandedLearner(testOnSentences: Boolean = false): Unit = {
    // since baselineLearner is used in expandedLearner, first train the baseline
    trainAndTestLearner(baselineLearner, 1, test = false, testOnSentences = false, saveModel = true)
    trainAndTestLearner(expandedLearner, 20, test = true, testOnSentences, saveModel = true)
  }

  def loadAndTestExpandedLearner(): Unit = {
    testLearner(baselineLearner, test = true, testOnSentences = false)
    testLearner(expandedLearner, test = true, testOnSentences = false)
  }

  def testLearnerWithSampleAristoQuestion(): Unit = {
    val q = " What force causes a feather to fall slower than a rock? (A) gravity (B) air resistance (C) magnetism (D) electricity"
    val maybeSplitQuestion = ParentheticalChoiceIdentifier(q)
    val multipleChoiceSelection = EssentialTermsUtils.fallbackDecomposer(maybeSplitQuestion)
    val aristoQuestion = Question(q, Some(maybeSplitQuestion.question), multipleChoiceSelection)
    val essentialTerms = getEssentialTermsForAristoQuestion(aristoQuestion, expandedLearner)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
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
    actorSystem.shutdown()
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
    dataModel.tokens.clear
    dataModel.tokens.populate(trainConstiuents)
    dataModel.tokens.populate(testConstituents, train = false)

    // train
    logger.debug(s"Training learner ${learner.getSimpleName} for $numIterations iterations")
    learner.learn(numIterations)

    if (saveModel) {
      logger.debug(s"Saving model ${learner.getSimpleName} at ${learner.lcFilePath()}")
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
    dataModel.tokens.clear
    dataModel.tokens.populate(testConstituents, train = false)

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

  private def sumTuple(a: (Double, Double, Double), b: (Double, Double, Double)): (Double, Double, Double) = {
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

    val hammingDistances = testReader.data.map { consIt =>
      consIt.map(cons => if (goldLabel(cons) != learner.predictLabel(cons)) 1 else 0).sum
    }
    logger.info("Average hamming distance = " + hammingDistances.sum * 1d / hammingDistances.size)

    testReader.data.slice(0, 30).foreach { consIt =>
      val numSen = consIt.head.getTextAnnotation.getNumberOfSentences
      (0 until numSen).foreach(id => logger.info(consIt.head.getTextAnnotation.getSentence(id).toString))

      val goldImportantSentence = consIt.map { cons => cons.getSurfaceForm }.mkString("//")
      val gold = consIt.map(cons => convertToZeroOne(goldLabel(cons))).toSeq
      val predicted = consIt.map(cons => convertToZeroOne(learner.predictLabel(cons))).toSeq
      val goldStr = gold.mkString("")
      val predictedStr = predicted.mkString("")
      val hammingDistance = (gold diff predicted).size * 1d / predicted.size
      logger.info(goldImportantSentence)
      logger.info(goldStr)
      logger.info(predictedStr)
      logger.info(s"hamming distance = $hammingDistance")
      logger.info("----")
    }

    logger.info("===  exact prediction ")
    testReader.data.foreach { consIt =>
      val gold = consIt.map(goldLabel(_)).mkString
      val predicted = consIt.map(learner(_)).mkString

      val fakePred = if (gold == predicted) "same" else "different"
      testerExact.reportPrediction(fakePred, "same")
    }
    testerExact.printPerformance(System.out)
  }
}

/** An EssentialTermsApp companion object with main() method. */
object EssentialTermsApp extends Logging {
  def main(args: Array[String]): Unit = {
    val usageStr = "\nUSAGE: run 1 (TrainAndTestMainLearner) | 2 (LoadAndTestMainLearner) | " +
      "3 (TrainAndTestBaseline) | 4 (TestWithAristoQuestion) | 5 (CacheSalienceScores)"
    if (args.isEmpty || args.length > 1) {
      throw new IllegalArgumentException(usageStr)
    } else {
      val testType = args(0)
      testType match {
        case "1" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false)
          essentialTermsApp.trainAndTestExpandedLearner(testOnSentences = false)
        case "2" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true)
          essentialTermsApp.loadAndTestExpandedLearner()
        case "3" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false)
          essentialTermsApp.trainAndTestBaselineLearner(testOnSentences = false)
        case "4" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false)
          essentialTermsApp.cacheSalienceScoresInRedis()
        case "5" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true)
          essentialTermsApp.testLearnerWithSampleAristoQuestion()
        case _ =>
          throw new IllegalArgumentException(s"Unrecognized run option; $usageStr")
      }
    }
  }
}
