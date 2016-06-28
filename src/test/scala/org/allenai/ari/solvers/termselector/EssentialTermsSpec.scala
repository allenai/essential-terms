package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.EssentialTermsSensors._
import org.allenai.ari.solvers.termselector.evaluation.Evaluator
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.common.testkit.UnitSpec

/** Test overall functionality of the TableILP solver */
class EssentialTermsSpec extends UnitSpec {
  //TODO: ignored due to some weird behavior in Semaphore. Address it in future.
  "Lemma baseline " should "should correctly work and have at least 74 F1" in {
    val (baselineDataModelTrain, baselineLearnersTrain) = BaselineLearner.makeNewLearners("loadPreTrained", "train")
    // load the data into the model
    baselineDataModelTrain.essentialTermTokens.populate(testConstituents, train = false)
    val evaluator = new Evaluator(baselineLearnersTrain.lemma)
    val scoreMap = evaluator.test(testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    assert(scoreMap(Constants.IMPORTANT_LABEL)._1 >= 0.74)
  }

  //TODO: ignored due to some weird behavior in Semaphore. Address it in future.
  "Expanded classifier " should " should correctly work and have at least 80 F1" in {
    val salienceBaselines = SalienceLearner.makeNewLearners()
    val (baselineDataModel, baselineClassifiers) = BaselineLearner.makeNewLearners(loadModelType = "loadPreTrained", "dev")
    val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(loadModelType = "loadPreTrained", "SVM",
      baselineClassifiers, baselineDataModel, salienceBaselines)
    // load the data into the model
    expandedDataModel.essentialTermTokens.populate(testConstituents, train = false)
    val evaluator = new Evaluator(expandedLearner)
    val scoreMap = evaluator.test(testConstituents, Constants.EXPANDED_LEARNER_THRESHOLD, 1.0)
    assert(scoreMap(Constants.IMPORTANT_LABEL)._1 >= 0.80)
  }
}