package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.Sensors._
import org.allenai.ari.solvers.termselector.evaluation.Evaluator
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.common.testkit.UnitSpec

/** Test overall functionality of the TableILP solver */
class EssentialTermsSpec extends UnitSpec {
  "Lemma baseline " should "should correctly work and have at least 74 F1" in {
    val (baselineDataModelTrain, baselineLearnersTrain) =
      BaselineLearners.makeNewLearners(LoadFromDatastore, "train")
    // load the data into the model
    baselineDataModelTrain.essentialTermTokens.populate(testConstituents, train = false)
    val evaluator = new Evaluator(baselineLearnersTrain.lemma)
    val scoreMap = evaluator.test(testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be >= 0.74
  }

  "Salience baseline " should "should correctly work and have at least 67 F1" in {
    val maxSalienceBaseline = SalienceLearner.makeNewLearners().max
    val evaluator = new Evaluator(maxSalienceBaseline)
    val scoreMap = evaluator.test(testConstituents, Constants.LEMMA_BASELINE_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be >= 0.67
  }

  "Expanded classifier " should " should correctly work and have at least 80 F1" in {
    val salienceBaselines = SalienceLearner.makeNewLearners()
    val (baselineDataModel, baselineClassifiers) =
      BaselineLearners.makeNewLearners(LoadFromDatastore, "dev")
    val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(LoadFromDatastore, "SVM",
      baselineClassifiers, baselineDataModel, salienceBaselines)
    // load the data into the model
    expandedDataModel.essentialTermTokens.populate(testConstituents, train = false)
    val evaluator = new Evaluator(expandedLearner)
    val scoreMap = evaluator.test(testConstituents, Constants.EXPANDED_LEARNER_THRESHOLD, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be >= 0.80
  }

  "Expanded classifier for direct answer questions " should " should correctly work and have at least 79.5 F1" in {
    val (baselineDataModel, baselineClassifiers) =
      BaselineLearners.makeNewLearners(LoadFromDatastore, "dev")
    val dummySalienceLearner = new DummySalienceLearner(baselineDataModel)
    val salienceBaselines = SalienceLearners(dummySalienceLearner, dummySalienceLearner)
    val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(LoadFromDatastore, "SVM-directAnswer",
      baselineClassifiers, baselineDataModel, salienceBaselines)
    // load the data into the model
    expandedDataModel.essentialTermTokens.populate(testConstituents, train = false)
    val evaluator = new Evaluator(expandedLearner)
    val scoreMap = evaluator.test(testConstituents, Constants.EXPANDED_LEARNER_THRESHOLD_DIRECT_ANSWER, 1.0)
    val f1Score = scoreMap(Constants.IMPORTANT_LABEL)._1
    f1Score should be >= 0.795
  }
}
