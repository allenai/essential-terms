package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.Sensors
import org.allenai.ari.solvers.termselector.params.LearnerParams
import org.allenai.common.Logging

/** A collection of baseline learners. */
case class BaselineLearners(
  surfaceForm: BaselineLearner,
  lemma: BaselineLearner,
  posConjLemma: BaselineLearner,
  wordFormConjNer: BaselineLearner,
  wordFormConjNerConjPos: BaselineLearner,
  baselineLearnerLemmaPair: BaselineLearner
)

object BaselineLearners extends Logging {

  /** Make a collection of [[BaselineLearner]]s. Also return the underlying data model.
    * @param loadModelType load the pre-trained model, or the one on disk, or don't load any model
    * @return a baseline data model and a bunch of baseline learners
    */
  def makeNewLearners(sensors: Sensors, learnerParams: LearnerParams, classifierModel: String, loadModelType: LoadType): (BaselineDataModel, BaselineLearners) = {
    // the baselines are trained either on train or test
    require(classifierModel == "dev" || classifierModel == "train")

    // create the baseline data model and the corresponding learner objects
    // the baseline data model contains information about how different elements of the problem
    // are connected to each other (in this case, the output label of an arbitrary input instance
    // in the training data)
    val baselineDataModel = new BaselineDataModel(sensors)

    val baselineLearnerSurfaceForm = BaselineLearner.makeNewLearner(
      learnerParams,
      baselineDataModel,
      baselineDataModel.wordForm, baselineDataModel.goldLabel,
      "surfaceForm-" + classifierModel, loadModelType
    )
    val baselineLearnerLemma = BaselineLearner.makeNewLearner(
      learnerParams,
      baselineDataModel,
      baselineDataModel.lemma, baselineDataModel.goldLabel,
      "lemma-" + classifierModel, loadModelType
    )
    val baselineLearnerPosConjLemma = BaselineLearner.makeNewLearner(
      learnerParams,
      baselineDataModel,
      baselineDataModel.posConjLemma, baselineDataModel.goldLabel,
      "PosConjLemma-" + classifierModel, loadModelType
    )
    val baselineLearnerWordFormConjNer = BaselineLearner.makeNewLearner(
      learnerParams,
      baselineDataModel,
      baselineDataModel.wordFormConjNer, baselineDataModel.goldLabel,
      "baselineLearnerWordFormConjNer-" + classifierModel, loadModelType
    )
    val baselineLearnerWordFormConjNerCojPos = BaselineLearner.makeNewLearner(
      learnerParams,
      baselineDataModel,
      baselineDataModel.wordFormConjNerConjPOS, baselineDataModel.goldLabel,
      "baselineLearnerWordFormConjNerCojPos-" + classifierModel, loadModelType
    )
    val baselineLearnerLemmaPair = BaselineLearner.makeNewLearner(
      learnerParams,
      baselineDataModel,
      baselineDataModel.lemmaPair, baselineDataModel.goldLabelPair,
      "baselineLearnerLemmaPair-" + classifierModel, loadModelType
    )

    val baselineLearners = BaselineLearners(baselineLearnerSurfaceForm, baselineLearnerLemma,
      baselineLearnerPosConjLemma, baselineLearnerWordFormConjNer,
      baselineLearnerWordFormConjNerCojPos, baselineLearnerLemmaPair)

    (baselineDataModel, baselineLearners)
  }
}
