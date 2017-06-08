package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.evaluation.Evaluator
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.ari.solvers.termselector.params.LearnerParams
import org.allenai.common.testkit.ActorSpec

import akka.actor.ActorSystem
import edu.illinois.cs.cogcomp.lbjava.classify.TestDiscrete
import org.scalatest.OneInstancePerTest

/** Test overall functionality of the TableILP solver */
class EssentialTermsSpec extends ActorSpec(ActorSystem("essential-terms-spec")) with OneInstancePerTest {
  lazy val lookupLearnerService = EssentialTermsService("Lookup")
  lazy val commonSensors = lookupLearnerService.sensors

  def getServiceF1GivenLearnerType(classifierType: String, classifierModel: String = ""): Double = {
    val learnerService = EssentialTermsService(classifierType, classifierModel, commonSensors)
    val tester = new TestDiscrete
    learnerService.sensors.allQuestions.slice(0, 100).foreach { q =>
      val (goldEssentialTerms, goldEssentialTermScoreMap) = lookupLearnerService.getEssentialTermsAndScores(q.aristoQuestion)
      val predictedTerms = learnerService.getEssentialTerms(q.aristoQuestion)
      goldEssentialTermScoreMap.keys.foreach { term =>
        tester.reportPrediction(predictedTerms.contains(term).toString, goldEssentialTerms.contains(term).toString)
      }
    }
    tester.getF1("true")
  }

  "Essentialterms service" should "work for LemmaBaseline" in {
    val f1Score = getServiceF1GivenLearnerType("LemmaBaseline")
    f1Score should be(0.786 +- 0.02)
  }

  it should "work for MaxSalience" in {
    val f1Score = getServiceF1GivenLearnerType("MaxSalience")
    f1Score should be(0.749 +- 0.02)
  }

  it should "work for expanded-classifier" in {
    val f1Score = getServiceF1GivenLearnerType("Expanded", "SVM")
    f1Score should be(0.819 +- 0.02)
  }

  /** the following tests contains testing the systems directly (not through the service)
    * It should be noted that this is NOT the standard way of using a solver
    */
  "Lemma baseline " should "correctly work and have at least 74 F1" in {
    val (baselineDataModelTrain, baselineLearnersTrain) =
      BaselineLearners.makeNewLearners(commonSensors, LearnerParams.default, "train", LoadFromDatastore)

    // load the data into the model
    baselineDataModelTrain.essentialTermTokens.populate(commonSensors.testConstituents, train = false)
    val evaluator = new Evaluator(baselineLearnersTrain.lemma, commonSensors)
    val scoreMap = evaluator.test(commonSensors.testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be(0.74 +- 0.02)
  }

  "Salience baseline " should "correctly work and have at least 67 F1" in {
    val maxSalienceBaseline = SalienceLearner.makeNewLearners(commonSensors, directAnswerQuestions = false).max
    val evaluator = new Evaluator(maxSalienceBaseline, commonSensors)
    val scoreMap = evaluator.test(commonSensors.testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be(0.67 +- 0.02)
  }

  "Expanded classifier " should "correctly work and have at least 80 F1" in {
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

  it should "correctly work for direct answer questions and have at least 79.5 F1" in {
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
