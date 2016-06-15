package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.EssentialTermsSensors._
import org.allenai.common.Logging
import com.redis._
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.parser.LBJIteratorParserScala
import edu.illinois.cs.cogcomp.lbjava.learn.{ SparseNetworkLearner, SupportVectorMachine }
import spray.json._
import DefaultJsonProtocol._

import scala.collection.JavaConverters._
import scala.language.postfixOps

import java.io.{ File, PrintWriter }

/** A sample application to train, test, save, and load essential terms classifiers. */
class EssentialTermsApp(loadSavedModel: Boolean, classifierModel: String) extends Logging {
  // lazily create the baseline and expanded data models and learners
  private lazy val (baselineDataModel, baselineLearners) = BaselineLearner.makeNewLearners(loadSavedModel)
  private lazy val salienceLearners = SalienceBaseline.makeNewLearners()
  private lazy val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(
    loadSavedModel,
    classifierModel, baselineLearners, baselineDataModel, salienceLearners
  )
  private lazy val constrainedLearner = ConstrainedLearner.makeNewLearner(expandedLearner, expandedDataModel)

  def trainAndTestBaselineLearners(test: Boolean = true, testOnSentences: Boolean = false, trainOnDev: Boolean): Unit = {
    trainAndTestLearner(baselineLearners.surfaceForm, 1, test,
      testOnSentences, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.lemma, 1, test,
      testOnSentences, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.posConjLemma, 1, test,
      testOnSentences, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.wordFormConjNer, 1, test,
      testOnSentences, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.wordFormConjNerConjPos, 1, test,
      testOnSentences, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.baselineLearnerLemmaPair, 1, test,
      testOnSentences, saveModel = true, trainOnDev)
  }

  def trainAndTestExpandedLearner(testOnSentences: Boolean = false): Unit = {
    // since baselineLearners is used in expandedLearner, first train the baselines
    trainAndTestBaselineLearners(testOnSentences = false, trainOnDev = true)
    //loadAndTestExpandedLearner()
    trainAndTestLearner(expandedLearner, 20, test = true, testOnSentences, saveModel = true)
    val featureLength = expandedLearner.classifier.getPrunedLexiconSize
    println("Feature length = " + featureLength)
  }

  def loadAndTestExpandedLearner(): Unit = {
    testLearner(baselineLearners.surfaceForm, test = true, testOnSentences = true)
    testLearner(baselineLearners.lemma, test = true, testOnSentences = true)
    testLearner(baselineLearners.posConjLemma, test = true, testOnSentences = false)
    testLearner(baselineLearners.wordFormConjNer, test = true, testOnSentences = false)
    testLearner(baselineLearners.wordFormConjNerConjPos, test = true, testOnSentences = false)
    testLearner(expandedLearner, test = true, testOnSentences = true)
  }

  def testSalienceLearner(salienceType: String): Unit = {
    val salienceLearner = salienceType match {
      case "max" => salienceLearners.max
      case "sum" => salienceLearners.sum
    }
    testLearner(salienceLearner, test = false, testOnSentences = true)
  }

  def testLearnerWithSampleAristoQuestion(): Unit = {
    val q = "In New York State, the longest period of daylight occurs during which month? (A) " +
      "December (B) June (C) March (D) September"
    //    val q = " What force causes a feather to fall slower than a rock? " +
    //      "(A) gravity (B) air resistance (C) magnetism (D) electricity"
    val aristoQuestion = decomposeQuestion(q)
    val essentialTerms = getEssentialTermsForAristoQuestion(aristoQuestion, expandedLearner, threshold = EssentialTermsConstants.EXPANDED_UP_THRESHOLD)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
    logger.info(expandedLearner.getEssentialTermScores(aristoQuestion).toString)
  }

  def testConstrainedLearnerWithSampleAristoQuestion(): Unit = {
    val q = "In New York State, the longest period of daylight occurs during which month? (A) " +
      "December (B) June (C) March (D) September"
    //    val q = " What force causes a feather to fall slower than a rock? " +
    //      "(A) gravity (B) air resistance (C) magnetism (D) electricity"
    val aristoQuestion = decomposeQuestion(q)
    val essentialTerms = getEssentialTermsForAristoQuestionConstrainedLearner(aristoQuestion, expandedDataModel, constrainedLearner)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
  }

  def testSalienceWithSampleAristoQuestion(salienceType: String): Unit = {
    val q = "A student is growing some plants for an experiment. She notices small white spots on the leaves. " +
      "Which tool should she use to get a better look at the spots? " +
      "(A) thermometer  (B) hand lens  (C) graduated cylinder  (D) balance "
    //      "In New York State, the longest period of daylight occurs during which month? (A) " +
    //      "December (B) June (C) March (D) September"
    val aristoQuestion = decomposeQuestion(q)
    val (salienceLearner, th) = salienceType match {
      case "max" => (salienceLearners.max, EssentialTermsConstants.MAX_SALIENCE_UP_THRESHOLD)
      case "sum" => (salienceLearners.sum, EssentialTermsConstants.SUM_SALIENCE_UP_THRESHOLD)
    }
    val scores = salienceLearner.getEssentialTermScores(aristoQuestion)
    logger.debug("Identified essentiality scores: " + scores.toString)
    val essentialTerms = salienceLearner.getEssentialTerms(aristoQuestion, th)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
  }

  def cacheSalienceScoresForAllQuestionsInRedis(): Unit = {
    allQuestions.foreach { q => getSalienceScores(q.aristoQuestion) }
    actorSystem.terminate()
  }

  /** saving the salience cache of the questions in the training data */
  def saveSalienceCacheOnDisk(): Unit = {
    val r = new RedisClient("localhost", 6379)
    val writer = new PrintWriter(new File(EssentialTermsConstants.SALIENCE_CACHE))
    allQuestions.foreach { q =>
      if (r.exists(q.rawQuestion) && q.aristoQuestion.selections.nonEmpty) {
        writer.write(s"${q.rawQuestion}\n${r.get(q.rawQuestion).get}\n")
      } else {
        logger.debug(" ===> Skipping . . . ")
      }
    }
    writer.close()
  }

  /** saving all the salience annotations in the cache */
  def saveRedisAnnotationCache(): Unit = {
    val keys = annotationRedisCache.keys().get
    logger.info(s"Saving ${keys.size} elements in the cache. ")
    val writer = new PrintWriter(new File(EssentialTermsConstants.SALIENCE_CACHE))
    keys.foreach { key =>
      val value = if (key.isDefined && key.get.contains(EssentialTermsConstants.SALIENCE_PREFIX)) {
        annotationRedisCache.get(key.get)
      } else {
        None
      }
      if (value.isDefined && key.isDefined) {
        writer.write(s"${key.get}\n${value.get}\n")
      }
    }
    writer.close()
  }

  private def trainAndTestLearner(
    learner: IllinoisLearner,
    numIterations: Int,
    test: Boolean = true,
    testOnSentences: Boolean = true,
    saveModel: Boolean = false,
    trainOnDev: Boolean = false
  ): Unit = {
    val dataModel = learner.dataModel
    // load the data into the model
    dataModel.essentialTermTokens.clear
    val targetConstituents = if (trainOnDev) devConstituents else trainConstituents
    dataModel.essentialTermTokens.populate(targetConstituents)
    dataModel.essentialTermTokens.populate(testConstituents, train = false)

    println("training set inside DM: " + expandedDataModel.essentialTermTokens.trainingSet.size)
    println("testing set inside DM: " + expandedDataModel.essentialTermTokens.testingSet.size)

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
    testOnSentences: Boolean,
    testOnTraining: Boolean = false
  ): Unit = {
    val dataModel = learner.dataModel
    // load the data into the model
    dataModel.essentialTermTokens.clear

    val constituents = if (testOnTraining) trainConstituents else testConstituents

    // test
    if (test) {
      logger.debug(s"Testing learner ${learner.getSimpleName}")
      learner.test(constituents)
    }
    // microAvgTest(baselineLearner)
    if (testOnSentences) {
      logger.debug(s"Testing learner ${learner.getSimpleName} over sentences")
      learner.rankingMeasures()
    }
  }

  def printAllFeatures() = {
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](trainSentences ++ testSentences)
    testReader.reset()

    // one time dry run, to add all the lexicon
    testReader.data.foreach { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala
      cons.foreach { c => expandedLearner.classifier.getExampleArray(c, true) }
    }

    val featureLength = expandedLearner.classifier.getPrunedLexiconSize
    println("Feature length = " + featureLength)
    printFeatures(train = true)
    printFeatures(train = false)
  }

  /* this would print the feature values on disk */
  private def printFeatures(train: Boolean): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(s"src/main/resources/outputFeatures_${if (train) "train" else "test"}.arff"))
    val featureLength = expandedLearner.classifier.getPrunedLexiconSize

    pw.write("@RELATION EssentialTerms\n")
    (0 until featureLength).foreach { idx => pw.write(s"@ATTRIBUTE $idx NUMERIC\n") }
    pw.write("@ATTRIBUTE f60531 {IMPORTANT, NOT-IMPORTANT}\n")
    pw.write("@DATA\n")

    val goldLabel = expandedLearner.dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](if (train) trainSentences else testSentences)
    testReader.reset()

    testReader.data.foreach { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala
      cons.foreach { c =>
        val out = expandedLearner.classifier.getExampleArray(c, true)
        val intArray = out(0).asInstanceOf[Array[Int]].toList
        val doubleArray = out(1).asInstanceOf[Array[Double]].toList

        pw.write("{")
        val featureValues = intArray.zip(doubleArray).groupBy { _._1 }.map { _._2.head }.toList. // remove the repeated features
          filter { case (ind, value) => value != 0.0 }. // drop zero features
          sortBy { case (ind, value) => ind }.
          map {
            case (ind, value) => ind + " " + (if (value == 1.0) "1" else value) // print feature as integer if it is 1.0
          }.mkString(", ")
        pw.write(featureValues)
        pw.write(", " + featureLength + " " + goldLabel(c) + "}\n")
      }
    }
    pw.close()
  }

  def tuneClassifierThreshold(classifier: String): Unit = {
    val c = classifier match {
      case "maxSalience" => salienceLearners.max
      case "sumSalience" => salienceLearners.sum
      case "wordBaseline" => baselineLearners.surfaceForm
      case "lemmaBaseline" => baselineLearners.lemma
      case "expanded" => expandedLearner
    }

    testWithAlpha(0.1)
    testWithAlpha(0.3)
    testWithAlpha(0.5)
    testWithAlpha(0.7)
    testWithAlpha(0.9)
    testWithAlpha(1.0)
    testWithAlpha(1.1)
    testWithAlpha(1.3)
    testWithAlpha(1.5)
    testWithAlpha(1.7)
    testWithAlpha(1.9)

    def testWithAlpha(alpha: Double): Unit = {
      println("-------")
      val threshold = c.tuneThreshold(alpha)
      val trainScore = c.testAcrossSentences(devSentences, threshold, alpha)
      println("train = " + trainScore.get(EssentialTermsConstants.IMPORTANT_LABEL))
      val testScores = c.testAcrossSentences(threshold, alpha)
      println("test = " + testScores.get(EssentialTermsConstants.IMPORTANT_LABEL))
      c.hammingMeasure(threshold)
    }

    val featureLength = expandedLearner.classifier.getPrunedLexiconSize
    println("Feature length = " + featureLength)
  }

  def printMistakes(): Unit = {
    expandedLearner.printMistakes(0.2)
  }
}

/** An EssentialTermsApp companion object with main() method. */
object EssentialTermsApp extends Logging {
  def main(args: Array[String]): Unit = {
    //        println(salienceMap.size)
    //        println(allQuestions.size)
    //        println(allQuestions.map{_.numAnnotators.get }.toSet)
    //        println(allQuestions.count{_.numAnnotators.get == 10 })
    //        println(allQuestions.count{_.numAnnotators.get > 4 })
    //        println(allQuestions.count{_.numAnnotators.get == 5 })
    //        println(allQuestions.count{_.numAnnotators.get == 4 })
    //        println(allQuestions.count{_.numAnnotators.get == 3 })
    //        println(allQuestions.count{_.numAnnotators.get == 2 })
    //
    //        println(trainSentences.size)
    //        println(testSentences.size)
    //
    //        val a = allConstituents.toList.groupBy{ _.getConstituentScore }.map{ case (a,b) => (a, b.size)}.toList.sortBy{ case (a,b) => a }
    //        println(a)
    //
    //        a.foreach{ case (b,c) => print(b + "\t" + c + "\n")   }
    //
    //        a.foreach{ case (c,b) => print(c+ "\t" )   }
    //        println("\n")
    //        a.foreach{ case (c,b) => print(b+ "\t" )   }
    //
    //        println(allConstituents.size)
    //        //

    // whatQuestions, whichQuestions, whereQuestions, whenQuestions, howQuestions, nonWhQuestions
    println("all test questions = " + testSentences.size)
    println("all train questions = " + trainSentences.size)
    println("all test constituents = " + testConstituents.size)
    println("all train constituents = " + trainConstituents.size)
    //    println("whatQuestions = " + whatQuestions.size)
    //    println("whichQuestions= " + whichQuestions.size)
    //    println("whereQuestions = " + whereQuestions.size)
    //    println("whenQuestions = " + whenQuestions.size)
    //    println("howQuestions = " + howQuestions.size)
    //    println("nonWhQuestions = " + nonWhQuestions.size)

    println("size of salience map" + salienceMap.size)

    val usageStr = "\nUSAGE: run 1 (TrainAndTestMainLearner) | 2 (LoadAndTestMainLearner) | " +
      "3 (TrainAndTestBaseline) | 4 (TestWithAristoQuestion) | 5 (TestConstrainedLearnerWithAristoQuestion) | " +
      "6 (CacheSalienceScores) | 7 (PrintMistakes) | 8 (PrintFeatures) | " +
      "9 (TestSalienceBaslineWithAristoQuestion) <classifier model>"
    if (args.isEmpty || args.length > 2) {
      throw new IllegalArgumentException(usageStr)
    } else {
      val testType = args(0)
      val arg1 = args.lift(1).getOrElse("")
      testType match {
        case "1" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false, arg1)
          essentialTermsApp.trainAndTestExpandedLearner(testOnSentences = false)
        case "2" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, arg1)
          essentialTermsApp.loadAndTestExpandedLearner()
        case "3" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false, arg1)
          essentialTermsApp.trainAndTestBaselineLearners(test = true, testOnSentences = false, trainOnDev = false)
        case "4" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, arg1)
          essentialTermsApp.testLearnerWithSampleAristoQuestion()
        case "5" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, arg1)
          essentialTermsApp.testConstrainedLearnerWithSampleAristoQuestion()
        case "6" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false, arg1)
          essentialTermsApp.cacheSalienceScoresForAllQuestionsInRedis()
        case "7" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, arg1)
          essentialTermsApp.printMistakes()
        case "8" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, arg1)
          essentialTermsApp.printAllFeatures()
        case "9" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "SVM")
          essentialTermsApp.testSalienceWithSampleAristoQuestion(arg1)
        case "10" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "SVM")
          essentialTermsApp.testSalienceLearner(arg1)
        case "11" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "SVM")
          essentialTermsApp.tuneClassifierThreshold(arg1)
        case "12" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "SVM")
          essentialTermsApp.saveRedisAnnotationCache()
        case _ =>
          throw new IllegalArgumentException(s"Unrecognized run option; $usageStr")
      }
    }
    actorSystem.terminate()
  }
}
