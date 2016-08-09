package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.evaluation.Evaluator
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.ari.solvers.termselector.params.LearnerParams
import org.allenai.common.testkit.UnitSpec

import akka.actor.ActorSystem
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete

/** Test overall functionality of the TableILP solver */
class EssentialTermsSpec extends UnitSpec {
  implicit val system = ActorSystem("ari-http-solver")
  val lookupLearner = InjectedLearnerAndThreshold("Lookup", "")
  val lookupLearnerService = new EssentialTermsService(lookupLearner)
  val commonSensors = lookupLearner.sensors
  val serviceParams = commonSensors.serviceParams

  def getServiceF1GivenLearnerType(classifierType: String, classifierModel: String = ""): Double = {
    val learner = InjectedLearnerAndThreshold(classifierType, classifierModel, commonSensors)
    val learnerService = new EssentialTermsService(learner)
    val tester = new TestDiscrete
    learner.sensors.allQuestions.slice(0, 100).foreach { q =>
      val (goldEssentialTerms, goldEssentialTermScoreMap) = lookupLearnerService.getEssentialTermsAndScores(q.aristoQuestion)
      val predictedTerms = learnerService.getEssentialTerms(q.aristoQuestion)
      goldEssentialTermScoreMap.keys.foreach { term =>
        tester.reportPrediction(predictedTerms.contains(term).toString, goldEssentialTerms.contains(term).toString)
      }
    }
    tester.getF1("true")
  }

  "Essentialterms service for lemma-baseline" should "work" in {
    val f1Score = getServiceF1GivenLearnerType("LemmaBaseline")
    f1Score should be(0.786 +- 0.02)
  }

  "Essentialterms service for salience-baseline" should "work" in {
    val f1Score = getServiceF1GivenLearnerType("MaxSalience")
    f1Score should be(0.749 +- 0.02)
  }

  "Essentialterms service for expanded-classifier" should "work" in {
    val f1Score = getServiceF1GivenLearnerType("Expanded", "SVM")
    f1Score should be(0.819 +- 0.02)
  }

  /** the following tests contains testing the systems directly (not through the service)
    * It should be noted that this is NOT the standard way of using a solver
    */
  "Lemma baseline " should "should correctly work and have at least 74 F1" in {
    val (baselineDataModelTrain, baselineLearnersTrain) =
      BaselineLearners.makeNewLearners(commonSensors, LearnerParams.default, "train", LoadFromDatastore)

    // load the data into the model
    baselineDataModelTrain.essentialTermTokens.populate(commonSensors.testConstituents, train = false)
    val evaluator = new Evaluator(baselineLearnersTrain.lemma, commonSensors)
    val scoreMap = evaluator.test(commonSensors.testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be(0.74 +- 0.02)
  }

  "Salience baseline " should "should correctly work and have at least 67 F1" in {
    val maxSalienceBaseline = SalienceLearner.makeNewLearners(commonSensors, directAnswerQuestions = false).max
    val evaluator = new Evaluator(maxSalienceBaseline, commonSensors)
    val scoreMap = evaluator.test(commonSensors.testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be(0.67 +- 0.02)
  }

  "Expanded classifier " should " should correctly work and have at least 80 F1" in {
    val salienceBaselines = SalienceLearner.makeNewLearners(commonSensors, directAnswerQuestions = false)
    val (baselineDataModel, baselineClassifiers) = BaselineLearners.makeNewLearners(
      commonSensors,
      LearnerParams.default, "dev", LoadFromDatastore
    )
    val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(
      commonSensors,
      LearnerParams.default, "SVM", LoadFromDatastore, baselineClassifiers, baselineDataModel, salienceBaselines
    )
    // load the data into the model
    expandedDataModel.essentialTermTokens.populate(commonSensors.testConstituents, train = false)
    val evaluator = new Evaluator(expandedLearner, commonSensors)
    val scoreMap = evaluator.test(commonSensors.testConstituents, Constants.EXPANDED_LEARNER_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be(0.80 +- 0.02)
  }

  "Expanded classifier for direct answer questions " should " should correctly work and have at least 79.5 F1" in {
    val (baselineDataModel, baselineClassifiers) = BaselineLearners.makeNewLearners(
      commonSensors,
      LearnerParams.default, "dev", LoadFromDatastore
    )
    val dummySalienceLearner = new DummySalienceLearner(baselineDataModel)
    val salienceBaselines = SalienceLearners(dummySalienceLearner, dummySalienceLearner)
    val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(commonSensors, LearnerParams.default,
      "SVM-directAnswer", LoadFromDatastore, baselineClassifiers, baselineDataModel, salienceBaselines)
    // load the data into the model
    expandedDataModel.essentialTermTokens.populate(commonSensors.testConstituents, train = false)
    val evaluator = new Evaluator(expandedLearner, commonSensors)
    val scoreMap = evaluator.test(commonSensors.testConstituents, Constants.EXPANDED_LEARNER_THRESHOLD_DIRECT_ANSWER, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be(0.795 +- 0.02)
  }
}
