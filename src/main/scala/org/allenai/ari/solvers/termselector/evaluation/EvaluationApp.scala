package org.allenai.ari.solvers.termselector.evaluation

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.params.{ ServiceParams, LearnerParams }
import org.allenai.ari.solvers.termselector.{ Sensors, Constants, Utils }
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.common.Logging

import com.redis.RedisClient
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.parser.IterableToLBJavaParser

import scala.collection.JavaConverters._
import scala.language.postfixOps

import java.io.{ File, PrintWriter }

/** A sample application to train, test, save, and load essential terms classifiers.
  * This code has small pieces for development/debugging/Training and has seen only light comments.
  */
class EvaluationApp(loadModelType: LoadType, classifierModel: String) extends Logging {
  private val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
  private val localConfig = rootConfig.getConfig("ari.solvers.termselector")

  val modifiedConfig = localConfig.withValue("classifierModel", ConfigValueFactory.fromAnyRef(classifierModel))

  val learnerParams = LearnerParams.fromConfig(modifiedConfig)
  val serviceParams = ServiceParams.fromConfig(modifiedConfig)

  val sensors = new Sensors(serviceParams)

  // lazily create the baseline and expanded data models and learners
  // baseline-train is used independently, while baseline-dev is used within expanded learner as feature
  private lazy val (baselineDataModelTrain, baselineLearnersTrain) =
    BaselineLearners.makeNewLearners(sensors, learnerParams, "train", loadModelType)
  private lazy val (baselineDataModelDev, baselineLearnersDev) =
    BaselineLearners.makeNewLearners(sensors, learnerParams, "dev", loadModelType)
  private lazy val salienceLearners = SalienceLearner.makeNewLearners(sensors, directAnswerQuestions = false)
  private lazy val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(sensors, learnerParams,
    loadModelType, baselineLearnersDev, baselineDataModelDev, salienceLearners)

  def trainAndTestBaselineLearners(test: Boolean = true, testRankingMeasures: Boolean = false, trainOnDev: Boolean): Unit = {
    val baselineLearners = if (trainOnDev) baselineLearnersDev else baselineLearnersTrain
    trainAndTestLearner(baselineLearners.surfaceForm, 1, test,
      testRankingMeasures, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.lemma, 1, test,
      testRankingMeasures, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.posConjLemma, 1, test,
      testRankingMeasures, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.wordFormConjNer, 1, test,
      testRankingMeasures, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.wordFormConjNerConjPos, 1, test,
      testRankingMeasures, saveModel = true, trainOnDev)
    trainAndTestLearner(baselineLearners.baselineLearnerLemmaPair, 1, test,
      testRankingMeasures, saveModel = true, trainOnDev)
  }

  def trainAndTestExpandedLearner(testOnSentences: Boolean = false): Unit = {
    // since baselineLearners is used in expandedLearner, first train the baselines
    trainAndTestBaselineLearners(testRankingMeasures = false, trainOnDev = true)
    trainAndTestLearner(expandedLearner, 20, test = true, testOnSentences, saveModel = true)
    val featureLength = expandedLearner.classifier.getPrunedLexiconSize
    logger.debug("Feature length = " + featureLength)
  }

  def loadAndTestExpandedLearner(): Unit = {
    testLearner(baselineLearnersDev.surfaceForm, test = true, testWithRankingMeasures = false)
    testLearner(baselineLearnersDev.lemma, test = true, testWithRankingMeasures = false)
    testLearner(baselineLearnersDev.posConjLemma, test = true, testWithRankingMeasures = false)
    testLearner(baselineLearnersDev.wordFormConjNer, test = true, testWithRankingMeasures = false)
    testLearner(baselineLearnersDev.wordFormConjNerConjPos, test = true, testWithRankingMeasures = false)
    testLearner(expandedLearner, test = true, testWithRankingMeasures = true)
  }

  def testSalienceLearner(salienceType: String): Unit = {
    val salienceLearner = salienceType match {
      case "max" => salienceLearners.max
      case "sum" => salienceLearners.sum
    }
    testLearner(salienceLearner, test = false, testWithRankingMeasures = true)
  }

  def testLearnerWithSampleAristoQuestion(): Unit = {
    val q = "In New York State, the longest period of daylight occurs during which month? (A) " +
      "December (B) June (C) March (D) September"
    val aristoQuestion = Utils.decomposeQuestion(q)
    val essentialTerms = expandedLearner.getEssentialTerms(aristoQuestion, threshold = Constants.EXPANDED_LEARNER_THRESHOLD)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
    logger.info(expandedLearner.getEssentialTermScores(aristoQuestion).toString)
  }

  def testLearnerWithSampleAristoQuestionDirectAnswer(): Unit = {
    val q = "In New York State, the longest period of daylight occurs during which month? "
    val aristoQuestion = Question(q, Some(q), Seq.empty)
    val essentialTerms = expandedLearner.getEssentialTerms(
      aristoQuestion,
      threshold = Constants.EXPANDED_LEARNER_THRESHOLD_DIRECT_ANSWER
    )
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
    logger.info(expandedLearner.getEssentialTermScores(aristoQuestion).toString)
  }

  def testSalienceWithSampleAristoQuestion(salienceType: String): Unit = {
    val q = "A student is growing some plants for an experiment. She notices small white spots on the leaves. " +
      "Which tool should she use to get a better look at the spots? " +
      "(A) thermometer  (B) hand lens  (C) graduated cylinder  (D) balance "
    val aristoQuestion = Utils.decomposeQuestion(q)
    val (salienceLearner, th) = salienceType match {
      case "max" => (salienceLearners.max, Constants.MAX_SALIENCE_THRESHOLD)
      case "sum" => (salienceLearners.sum, Constants.SUM_SALIENCE_THRESHOLD)
    }
    val scores = salienceLearner.getEssentialTermScores(aristoQuestion)
    logger.debug("Identified essentiality scores: " + scores.toString)
    val essentialTerms = salienceLearner.getEssentialTerms(aristoQuestion, th)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
  }

  def cacheSalienceScoresForAllQuestionsInRedis(): Unit = {
    sensors.allQuestions.foreach { q => sensors.annnotator.getSalienceScores(q.aristoQuestion) }
  }

  /** saving the salience cache of the questions in the training data */
  def saveSalienceCacheOnDisk(): Unit = {
    val r = new RedisClient("localhost", 6379)
    val writer = new PrintWriter(new File(Constants.SALIENCE_CACHE))
    sensors.allQuestions.foreach { q =>
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
    testWithRankingMeasures: Boolean = true,
    saveModel: Boolean = false,
    trainOnDev: Boolean = false
  ): Unit = {
    val dataModel = learner.dataModel
    // load the data into the model
    dataModel.essentialTermTokens.clear
    val targetConstituents = if (trainOnDev) sensors.devConstituents else sensors.trainConstituents
    dataModel.essentialTermTokens.populate(targetConstituents)
    dataModel.essentialTermTokens.populate(sensors.testConstituents, train = false)

    // train
    logger.debug(s"Training learner ${learner.getSimpleName} for $numIterations iterations")
    learner.learn(numIterations)

    if (saveModel) {
      logger.debug(s"Saving model ${learner.getSimpleName} at ${learner.lcFilePath}")
      learner.save()
    }

    testLearner(learner, test, testWithRankingMeasures)
  }

  private def testLearner(
    learner: IllinoisLearner,
    test: Boolean,
    testWithRankingMeasures: Boolean,
    testOnTraining: Boolean = false
  ): Unit = {
    val dataModel = learner.dataModel
    // load the data into the model
    dataModel.essentialTermTokens.clear

    val constituents = if (testOnTraining) sensors.trainConstituents else sensors.testConstituents

    // test
    if (test) {
      logger.debug(s"Testing learner ${learner.getSimpleName}")
      learner.test(constituents)
    }
    // microAvgTest(baselineLearner)
    if (testWithRankingMeasures) {
      logger.debug(s"Testing learner ${learner.getSimpleName} with ranking measures")
      val evaluator = new Evaluator(learner, sensors)
      evaluator.printRankingMeasures()
    }
  }

  def printAllFeatures() = {
    val testReader = new IterableToLBJavaParser[Iterable[Constituent]](sensors.allSentences)
    testReader.reset()

    // one time dry run, to add all the lexicon
    testReader.data.foreach { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(Constants.VIEW_NAME).getConstituents.asScala
      cons.foreach { c => expandedLearner.classifier.getExampleArray(c, true) }
    }

    val featureLength = expandedLearner.classifier.getPrunedLexiconSize
    println("Feature length = " + featureLength)
    printFeatures(train = true)
    printFeatures(train = false)
  }

  /* this would write the feature values on disk in '.arff' format. The generated features are sparse, meaning that
    * the features with zero values are ignored. */
  private def printFeatures(train: Boolean): Unit = {
    val suffix = if (train) "train" else "test"
    val pw = new PrintWriter(new File(s"outputFeatures_$suffix.arff"))
    val featureLength = expandedLearner.classifier.getPrunedLexiconSize

    pw.write("@RELATION EssentialTerms\n")
    (0 until featureLength).foreach { idx => pw.write(s"@ATTRIBUTE f$idx NUMERIC\n") }
    pw.write("@ATTRIBUTE class {IMPORTANT, NOT-IMPORTANT}\n")
    pw.write("@DATA\n")

    val goldLabel = expandedLearner.dataModel.goldLabel
    val targetSentences = if (train) sensors.trainSentences else sensors.testSentences
    val exampleReader = new IterableToLBJavaParser[Iterable[Constituent]](targetSentences)
    exampleReader.reset()

    exampleReader.data.foreach { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(Constants.VIEW_NAME).getConstituents.asScala
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

  def tuneClassifierThreshold(learnerName: String): Unit = {
    val learner = getClassifierGivenName(learnerName)
    val evaluator = new Evaluator(learner, sensors)
    val thresholdTuner = new ThresholdTuner(learner)

    // TODO(danielk): this is a little inefficient. We don't need to re-evaluate in on all the dataset from scratch; you
    // just need to evaluate once and reuse it later, with different threshold.
    def testWithAlpha(alphas: Seq[Double]): Unit = {
      alphas.foreach { alpha =>
        println("-------")
        val threshold = thresholdTuner.tuneThreshold(alpha)
        val trainScores = evaluator.testAcrossSentences(sensors.trainSentences, threshold, alpha)
        println("train = " + trainScores.get(Constants.IMPORTANT_LABEL))
        val testScores = evaluator.testAcrossSentences(sensors.testSentences, threshold, alpha)
        println("test = " + testScores.get(Constants.IMPORTANT_LABEL))
        evaluator.hammingMeasure(threshold)
      }
    }

    testWithAlpha((-30 to 8).map(x => math.exp(x / 5.0)))

    if (learner.isInstanceOf[ExpandedLearner]) {
      val featureLength = expandedLearner.classifier.getPrunedLexiconSize
      println("Feature length = " + featureLength)
    }
  }

  private def getClassifierGivenName(learnerName: String) = {
    learnerName match {
      case "maxSalience" => salienceLearners.max
      case "sumSalience" => salienceLearners.sum
      case "wordBaseline" => baselineLearnersTrain.surfaceForm
      case "lemmaBaseline" => baselineLearnersTrain.lemma
      case "expanded" => expandedLearner
      case name: String => throw new Exception(s"Wrong classisifer name $name!")
    }
  }

  /** saving all the salience annotations in the cache */
  def saveRedisAnnotationCache(): Unit = {
    val keys = sensors.annnotator.synchronizedRedisClient.keys("*")
    logger.info(s"Saving ${keys.size} elements in the cache. ")
    val writer = new PrintWriter(new File(Constants.SALIENCE_CACHE))
    keys.foreach { key =>
      if (key.contains(Constants.SALIENCE_PREFIX)) {
        sensors.annnotator.synchronizedRedisClient.get(key).foreach { value =>
          writer.write(s"$key\n$value\n")
        }
      }
    }
    writer.close()
  }

  def testClassifierAcrossThresholds(learnerName: String): Unit = {
    val learner = getClassifierGivenName(learnerName)
    val evaluator = new Evaluator(learner, sensors)

    def testWithAlpha(thresholds: Seq[Double]): Unit = {
      thresholds.foreach { threshold =>
        println("-------")
        val testScores = evaluator.testAcrossSentences(sensors.testSentences, threshold, 1)
        println("test = " + testScores.get(Constants.IMPORTANT_LABEL))
        evaluator.hammingMeasure(threshold)
      }
    }
    testWithAlpha(-0.1 to 1.1 by 0.05)

    if (learner.isInstanceOf[ExpandedLearner]) {
      val featureLength = expandedLearner.classifier.getPrunedLexiconSize
      println("Feature length = " + featureLength)
    }
  }

  def printMistakes(): Unit = {
    val evaluator = new Evaluator(expandedLearner, sensors)
    evaluator.printMistakes(0.2)
  }

  /** this file is meant to print important statistics related to the train/test data */
  def printStatistics(): Unit = {
    // the size of the salience cache
    println(sensors.salienceMap.size)
    // total number of questions (train and test)
    println(sensors.allQuestions.size)
    // total number of constituents (train and test)
    println(sensors.allConstituents.size)
    // distribution of questions across different number of annotations
    println(sensors.allQuestions.map { _.numAnnotators.get }.toSet)
    // number of questions with 10 annotators
    println(sensors.allQuestions.count { _.numAnnotators.get == 10 })
    // number of questions with 5 or more annotators
    println(sensors.allQuestions.count { _.numAnnotators.get >= 5 })
    // number of questions with 5 annotators
    println(sensors.allQuestions.count { _.numAnnotators.get == 5 })
    // number of questions with 4 annotators
    println(sensors.allQuestions.count { _.numAnnotators.get == 4 })
    // number of questions with 3 annotators
    println(sensors.allQuestions.count { _.numAnnotators.get == 3 })
    // number of questions with 2 annotators
    println(sensors.allQuestions.count { _.numAnnotators.get == 2 })
    // size of train and test sentences, respectively
    println("all test questions = " + sensors.testSentences.size)
    println("all train questions = " + sensors.trainSentences.size)
    println("all test constituents = " + sensors.testConstituents.size)
    println("all train constituents = " + sensors.trainConstituents.size)
    // size of: what questions, which questions, where questions, when questions, how questions, nonWh questions
    println("whatQuestions = " + sensors.whatQuestions.size)
    println("whichQuestions= " + sensors.whichQuestions.size)
    println("whereQuestions = " + sensors.whereQuestions.size)
    println("whenQuestions = " + sensors.whenQuestions.size)
    println("howQuestions = " + sensors.howQuestions.size)
    println("nonWhQuestions = " + sensors.nonWhQuestions.size)

    // group together the constituents with the same scores
    val scoreSizePairs = sensors.allConstituents.toList.groupBy { _.getConstituentScore }.map {
      case (score, constituents) => (score, constituents.size)
    }.toList.sortBy { case (score, _) => score }
    scoreSizePairs.foreach { case (score, size) => print(score + "\t" + size + "\n") }
  }
}

/** An EssentialTermsApp companion object with main() method. */
object EvaluationApp extends Logging {
  def main(args: Array[String]): Unit = {
    val usageStr = "\nUSAGE: " +
      "\n run 1  <classifier model>  (TrainAndTestMainLearner) " +
      "\n run 2  <classifier model> <train/dev>  (LoadAndTestMainLearner) " +
      "\n run 3  <classifier model>  (TrainAndTestBaseline) " +
      "\n run 4  <classifier model>  (TestWithAristoQuestion) " +
      "\n run 5  <classifier model>  (CacheSalienceScores) " +
      "\n run 6  <classifier model>  (PrintMistakes) " +
      "\n run 7  <classifier model>  (PrintFeatures) " +
      "\n run 8  <max/sum>           (TestSalienceBaslineWithAristoQuestion)" +
      "\n run 9 <max/sum>            (TestSalienceBasline)" +
      "\n run 10 <maxSalience/sumSalience/wordBaseline/lemmaBaseline/expanded> <classifier model>  (TuneClassifiers)" +
      "\n run 11 <classifier model>  (PrintStatistics)" +
      "\n run 12 <maxSalience/sumSalience/wordBaseline/lemmaBaseline/expanded> <classifier model>  (TestClassifierAcrossThresholds)" +
      "\n run 13 <classifier model>  (SaveRedisAnnotationCache)"

    if (args.length <= 0 || args.length > 3) {
      throw new IllegalArgumentException(usageStr)
    }
    val testType = args(0)
    val arg1 = args.lift(1).getOrElse("")
    val arg2 = args.lift(2).getOrElse("")
    val app = testType match {
      case "1" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = TrainModel, arg1)
        essentialTermsApp.trainAndTestExpandedLearner(testOnSentences = false)
        essentialTermsApp
      case "2" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.loadAndTestExpandedLearner()
        essentialTermsApp
      case "3" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = TrainModel, "")
        val trainOnDev = arg1 match {
          case "train" => false
          case "dev" => true
        }
        essentialTermsApp.trainAndTestBaselineLearners(test = true, testRankingMeasures = true, trainOnDev)
        essentialTermsApp
      case "4" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.testLearnerWithSampleAristoQuestion()
        essentialTermsApp
      case "5" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, "")
        essentialTermsApp.cacheSalienceScoresForAllQuestionsInRedis()
        essentialTermsApp
      case "6" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.printMistakes()
        essentialTermsApp
      case "7" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.printAllFeatures()
        essentialTermsApp
      case "8" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, "")
        essentialTermsApp.testSalienceWithSampleAristoQuestion(arg1)
        essentialTermsApp
      case "9" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, "")
        essentialTermsApp.testSalienceLearner(arg1)
        essentialTermsApp
      case "10" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.tuneClassifierThreshold(arg2)
        essentialTermsApp
      case "11" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.printStatistics()
        essentialTermsApp
      case "12" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.testClassifierAcrossThresholds(arg2)
        essentialTermsApp
      case "13" =>
        val essentialTermsApp = new EvaluationApp(loadModelType = LoadFromDatastore, arg1)
        essentialTermsApp.saveRedisAnnotationCache()
        essentialTermsApp
      case _ =>
        throw new IllegalArgumentException(s"Unrecognized run option; $usageStr")
    }
    if(app.sensors.actorSystemOpt.isDefined) app.sensors.actorSystemOpt.get.terminate()
  }
}

/*bject test {

  def main(args: Array[String]): Unit = {
    val learner = InjectedLearnerAndThreshold("Expanded", "SVM")
    println(learner)
    println(learner.sensors)
    println(learner.sensors.salienceMap.size)
    println(learner.sensors.constituentToAnnotationMap.size)
    println(learner.sensors.allConstituents.size)
    println(learner.sensors.allQuestions.size)
    println(learner.sensors.scienceTerms.size)
    println(learner.sensors.stopWords.size)
    //    val lookupLearner = InjectedLearnerAndThreshold("Lookup", "")
    //    val lookupLearnerService = new EssentialTermsService(lookupLearner)
    //    val learner = InjectedLearnerAndThreshold("Expanded", "SVM")
    val learnerService = new EssentialTermsService(learner)
    val tester = new TestDiscrete
    learner.sensors.allQuestions.slice(0, 3).foreach { q =>
      //      val (goldEssentialTerms, goldEssentialTermScoreMap) = lookupLearnerService.getEssentialTermsAndScores(q.aristoQuestion)
      //      val termScores = learnerService.getEssentialTermScores(q.aristoQuestion)
      val predictedTerms = learnerService.getEssentialTerms(q.aristoQuestion)
      println(predictedTerms)
      val predictedScores = learnerService.getEssentialTermScores(q.aristoQuestion)
      println(predictedScores)
      //      goldEssentialTermScoreMap.keys.foreach { term =>
      //        tester.reportPrediction(predictedTerms.contains(term).toString, goldEssentialTerms.contains(term).toString)
      //      }
    }
    //    val f1Score = tester.getF1("true")
    //    println(f1Score)
  }
}*/ 