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
class EssentialTermsApp(loadSavedModel: Boolean, classifierModel: String) extends Logging {
  // lazily create the baseline and expanded data models and learners
  private lazy val baselineLearners = BaselineLearner.makeNewLearners(loadSavedModel)_2
  private lazy val expandedLearner = ExpandedLearner.makeNewLearner(loadSavedModel, classifierModel)._4
  private lazy val (expandedDataModel, constrainedLearner) = ConstrainedLearner.makeNewLearner(classifierModel)
  private lazy val salienceLearner = SalienceBaseline.makeNewLearners()

  def trainAndTestBaselineLearners(testOnSentences: Boolean = false): Unit = {
    trainAndTestLearner(baselineLearners.surfaceForm, 1, test = true,
      testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.lemma, 1, test = true,
      testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.posConjLemma, 1, test = true,
      testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.wordFormConjNer, 1, test = true,
      testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.wordFormConjNerConjPos, 1, test = true,
      testOnSentences, saveModel = true)
    trainAndTestLearner(baselineLearners.baselineLearnerLemmaPair, 1, test = true,
      testOnSentences, saveModel = true)
  }

  def trainAndTestExpandedLearner(testOnSentences: Boolean = false): Unit = {
    // since baselineLearner is used in expandedLearner, first train the baseline
    trainAndTestBaselineLearners(testOnSentences)
    trainAndTestLearner(expandedLearner, 20, test = true, testOnSentences, saveModel = true)
  }

  def loadAndTestExpandedLearner(): Unit = {
    testLearner(baselineLearners.surfaceForm, test = true, testOnSentences = true)
    testLearner(baselineLearners.lemma, test = true, testOnSentences = true)
    testLearner(baselineLearners.posConjLemma, test = true, testOnSentences = false)
    testLearner(baselineLearners.wordFormConjNer, test = true, testOnSentences = false)
    testLearner(baselineLearners.wordFormConjNerConjPos, test = true, testOnSentences = false)
    testLearner(expandedLearner, test = true, testOnSentences = true)
  }

  def testSalienceLearner(): Unit = {
    testLearner(salienceLearner, test = false, testOnSentences = true)
  }

  def testLearnerWithSampleAristoQuestion(): Unit = {
    val q = "In New York State, the longest period of daylight occurs during which month? (A) " +
      "December (B) June (C) March (D) September"
    //    val q = " What force causes a feather to fall slower than a rock? " +
    //      "(A) gravity (B) air resistance (C) magnetism (D) electricity"
    val aristoQuestion = decomposeQuestion(q)
    val essentialTerms = getEssentialTermsForAristoQuestion(aristoQuestion, expandedLearner)
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

  def testSalienceWithSampleAristoQuestion(): Unit = {
    val q = "A student is growing some plants for an experiment. She notices small white spots on the leaves. " +
      "Which tool should she use to get a better look at the spots? " +
      "(A) thermometer  (B) hand lens  (C) graduated cylinder  (D) balance "
//      "In New York State, the longest period of daylight occurs during which month? (A) " +
//      "December (B) June (C) March (D) September"
    val aristoQuestion = decomposeQuestion(q)
    val scores = salienceLearner.getEssentialTermScores(aristoQuestion)
    logger.debug("Identified essentiality scores: " + scores.toString)
    val essentialTerms = salienceLearner.getEssentialTerms(aristoQuestion)
    logger.debug("Identified essential terms: " + essentialTerms.mkString("/"))
  }

  def cacheSalienceScoresForAllQuestionsInRedis(): Unit = {
    allQuestions.foreach { q => getSalienceScores(q.aristoQuestion) }
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

  private def convertToZeroOne(label: String): Int = {
    if (label == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
  }

  private def testOverSentences(learner: IllinoisLearner): Unit = {
    val goldLabel = learner.dataModel.goldLabel
    val testerExact = new TestDiscrete
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](testSentences)
    testReader.reset()

    // ranking-based measures
    if (!learner.isInstanceOf[BaselineLearner]) {
      // for BaselineLearner, "predictProbOfBeingEssential" is not defined
      val averagePrecisionList = testReader.data.map { consIt =>
        val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME)
          .getConstituents.asScala
        val goldLabelList = consIt.toList.map { cons =>
          if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
        }
        if (goldLabelList.sum <= 0) {
          logger.warn("no essential term in gold found in the gold annotation of this question .... ")
          logger.warn(s"question: ${consIt.head.getTextAnnotation.sentences().asScala.mkString("*")}")
          0.5
        } else {
          val scoreLabelPairs = consIt.toList.map { cons =>
            val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
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
      val predicted = consIt.map(learner.predictLabel(_)).mkString

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
          val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
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
    shortList.zipWithIndex.map { case (num, idx) => (longList(idx) + num) / 2 } ++
      longList.drop(shortList.length)
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
      val predicted = consList.map(cons => convertToZeroOne(expandedLearner.predictLabel(cons)))
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

  def printAllFeatures() = {
    val goldLabel = expandedLearner.dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](trainSentences ++ testSentences)
    testReader.reset()

    // one time dry run, to add all the lexicon
    testReader.data.foreach { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala
      cons.foreach { c => expandedLearner.classifier.getExampleArray(c, true) }
    }

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
            case (ind, value) => ind + " " + (if (value == 1.0) "1" else value) // print featur as intger if it is 1.0
          }.mkString(", ")
        pw.write(featureValues)
        pw.write(", " + featureLength + " " + goldLabel(c) + "}\n")
      }
    }
    pw.close()
  }

  def tuneClassifierThreshold(classifier: String): Unit = {
    tuneThreshold(salienceLearner)
  }

  /** given a set of training instances it returns the optimal threshold */
  private def tuneThreshold(learner: IllinoisLearner): Unit = {
    val goldLabel = learner.dataModel.goldLabel
    val testReader = new LBJIteratorParserScala[Iterable[Constituent]](trainSentences)
    testReader.reset()

    require(!learner.isInstanceOf[BaselineLearner], "The classifier should not be of type BaselineLearner  . . . ")

    // first get the real predictions per sentence
    val scoreLabelPairs = testReader.data.map { consIt =>
      val cons = consIt.head.getTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME)
        .getConstituents.asScala
      val goldLabelList = consIt.toList.map { cons =>
        if (goldLabel(cons) == EssentialTermsConstants.IMPORTANT_LABEL) 1 else 0
      }
      consIt.toList.map { cons =>
        val goldBinaryLabel = convertToZeroOne(goldLabel(cons))
        val predScore = learner.predictProbOfBeingEssential(cons)
        (predScore, goldBinaryLabel)
      }
    }

    // tune the threshold
    val initialThreshold = 0.5
    val totalIterations = 20
    val alpha = 1
    def stepSize(k: Int): Double = 0.2/(k+1)
    val (minTh, maxTh) = (scoreLabelPairs.map{a => a.unzip._1.min}.min, scoreLabelPairs.map{a => a.unzip._1.max}.max)
    val thresholdRange = (minTh to maxTh by 0.05)
    logger.info("thresholdRange : " + thresholdRange)

    // recursive
    def singleIteration(currentThreshold: Double, remainingIterations: Int, currentScore: Double): (Double, Double) = {
      if(remainingIterations == 0) {
        (currentThreshold, currentScore)
      } else {
        val thresholdUp = currentThreshold + stepSize(totalIterations  - remainingIterations)
        val thresholdDown = currentThreshold - stepSize(totalIterations  - remainingIterations)
        val FUp = evaluateFAllSentences(scoreLabelPairs , thresholdUp, alpha)._1
        val FDown = evaluateFAllSentences(scoreLabelPairs , thresholdDown, alpha)._1
        val thresholdScoresPairs = List((thresholdUp, FUp), (thresholdDown, FDown), (currentThreshold, currentScore))
        val (topTh, topF) = thresholdScoresPairs.sortBy(_._2).last
        logger.debug(s"Iteration:$remainingIterations / score: $currentScore / threshold: $topTh")
        singleIteration(topTh, remainingIterations - 1, topF)
      }
    }

    // try all points
    def tryGridOfThresholds(): (Double, (Double, Double, Double)) = {
      val scores = thresholdRange.map{ th => th -> evaluateFAllSentences(scoreLabelPairs , th, alpha) }
      println("all the scores " + scores)
      scores.sortBy{_._2._1}.last
    }

    def evaluateFAllSentences( scoreLabelPairs: Iterable[Seq[(Double, Int)]], threshold: Double, alpha: Double): (Double, Double, Double) = {
      val (fList, pList, rList) = scoreLabelPairs.map{ scoreLabelPairsPerSentence =>
        evaluateFSingleSentence(scoreLabelPairsPerSentence, threshold, alpha)
      }.toList.unzip3
      (fList.sum / fList.length, pList.sum / pList.length, rList.sum / rList.length)
    }

    def evaluateFSingleSentence( scoreLabelPairs: Seq[(Double, Int)], threshold: Double, alpha: Double): (Double, Double, Double)= {
      val testDiscrete = new TestDiscrete
      scoreLabelPairs.foreach{ case (score, label) =>
        testDiscrete.reportPrediction(if(score > threshold) "1" else "0", label.toString)
      }

//      val tmp = scoreLabelPairs.map{ case (score, label) =>
//        (if(score > threshold) "1" else "0", label.toString)
//      }
//      println("---------")
//      println("scoreLabelPairs = " + scoreLabelPairs)
//      println("threshold = "+ threshold)
//      println("labels = " + tmp)
//      println("P: " + testDiscrete.getPrecision("1") )
//      println("R: " + testDiscrete.getRecall("1") )
//      println("F1: " + testDiscrete.getF1("1") )
//      println("F: " + testDiscrete.getF(alpha, "1") )
//      println("Labels: " + testDiscrete.getLabels().toList.toString )
//      println("Predictions: " + testDiscrete.getPredictions.toList.toString )
//      println("Overall stats: " + testDiscrete.getOverallStats.toList.toString )

      //      println(scoreLabelPairs)
//      println(testDiscrete.getF(alpha, "1"))
      val f = testDiscrete.getF(alpha, "1")
      (if( f.isNaN ) 0 else f, testDiscrete.getPrecision("1"), testDiscrete.getRecall("1"))
    }

    val (topThreshold, topScore) = tryGridOfThresholds() // singleIteration(initialThreshold, totalIterations, -1)
    logger.info(s"Score after tuning: $topScore / threshold after tuning: $topThreshold")

  }
}

/** An EssentialTermsApp companion object with main() method. */
object EssentialTermsApp extends Logging {
  def main(args: Array[String]): Unit = {
    //    println(allQuestions.size)
    //    println(allQuestions.map{_.numAnnotators.get }.toSet)
    //    println(allQuestions.count{_.numAnnotators.get == 10 })
    //    println(allQuestions.count{_.numAnnotators.get > 4 })
    //    println(allQuestions.count{_.numAnnotators.get == 5 })
    //    println(allQuestions.count{_.numAnnotators.get == 4 })
    //    println(allQuestions.count{_.numAnnotators.get == 3 })
    //    println(allQuestions.count{_.numAnnotators.get == 2 })
    //
    //    println(trainSentences.size)
    //    println(testSentences.size)
    //
    //    val a = allConstituents.toList.groupBy{ _.getConstituentScore }.map{ case (a,b) => (a, b.size)}.toList.sortBy{ case (a,b) => a }
    //    println(a)
    //
    //    a.foreach{ case (b,c) => print(b + "\t" + c + "\n")   }
    //
    //    a.foreach{ case (c,b) => print(c+ "\t" )   }
    //    println("\n")
    //    a.foreach{ case (c,b) => print(b+ "\t" )   }
    //
    //    println(allConstituents.size)
    //

    val usageStr = "\nUSAGE: run 1 (TrainAndTestMainLearner) | 2 (LoadAndTestMainLearner) | " +
      "3 (TrainAndTestBaseline) | 4 (TestWithAristoQuestion) | 5 (TestConstrainedLearnerWithAristoQuestion) | " +
      "6 (CacheSalienceScores) | 7 (PrintMistakes) | 8 (PrintFeatures) | " +
      "9 (TestSalienceBaslineWithAristoQuestion) <classifier model>"
    if (args.isEmpty || args.length > 2) {
      throw new IllegalArgumentException(usageStr)
    } else {
      val testType = args(0)
      val classifierModel = args.lift(1).getOrElse("")
      testType match {
        case "1" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false, classifierModel)
          essentialTermsApp.trainAndTestExpandedLearner(testOnSentences = true)
        case "2" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, classifierModel)
          essentialTermsApp.loadAndTestExpandedLearner()
        case "3" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false, classifierModel)
          essentialTermsApp.trainAndTestBaselineLearners(testOnSentences = false)
        case "4" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, classifierModel)
          essentialTermsApp.testLearnerWithSampleAristoQuestion()
        case "5" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, classifierModel)
          essentialTermsApp.testConstrainedLearnerWithSampleAristoQuestion()
        case "6" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = false, classifierModel)
          essentialTermsApp.cacheSalienceScoresForAllQuestionsInRedis()
        case "7" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, classifierModel)
          essentialTermsApp.printMistakes()
        case "8" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "")
          essentialTermsApp.printAllFeatures()
        case "9" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "")
          essentialTermsApp.testSalienceWithSampleAristoQuestion()
        case "10" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "")
          essentialTermsApp.testSalienceLearner()
        case "11" =>
          val essentialTermsApp = new EssentialTermsApp(loadSavedModel = true, "")
          essentialTermsApp.tuneClassifierThreshold("")
        case _ =>
          throw new IllegalArgumentException(s"Unrecognized run option; $usageStr")
      }
    }
    actorSystem.terminate()
  }
}
