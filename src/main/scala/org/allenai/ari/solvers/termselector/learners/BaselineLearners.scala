package org.allenai.ari.solvers.termselector.learners

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
    *
    * @param loadModelType load the pre-trained model, or the one on disk, or don't load any model
    * @param classifierModel TODO(daniel) add description
    * @return a baseline data model and a bunch of baseline learners
    */
  def makeNewLearners(loadModelType: LoadType, classifierModel: String): (BaselineDataModel, BaselineLearners) = {
    // the baselines are trained either on train or test
    require(classifierModel == "dev" || classifierModel == "train")

    // create the baseline data model and the corresponding learner objects
    // the baseline data model contains informatiln about how different elements of the problem
    // are connected to each other (in this case, the output label of an arbitrary input instance
    // in the training data)
    val baselineDataModel = new BaselineDataModel

    val baselineLearnerSurfaceForm = BaselineLearner.makeNewLearner(
      baselineDataModel,
      baselineDataModel.wordForm, baselineDataModel.goldLabel,
      "surfaceForm-" + classifierModel, loadModelType
    )
    val baselineLearnerLemma = BaselineLearner.makeNewLearner(
      baselineDataModel,
      baselineDataModel.lemma, baselineDataModel.goldLabel,
      "lemma-" + classifierModel, loadModelType
    )
    val baselineLearnerPosConjLemma = BaselineLearner.makeNewLearner(
      baselineDataModel,
      baselineDataModel.posConjLemma, baselineDataModel.goldLabel,
      "PosConjLemma-" + classifierModel, loadModelType
    )
    val baselineLearnerWordFormConjNer = BaselineLearner.makeNewLearner(
      baselineDataModel,
      baselineDataModel.wordFormConjNer, baselineDataModel.goldLabel,
      "baselineLearnerWordFormConjNer-" + classifierModel, loadModelType
    )
    val baselineLearnerWordFormConjNerCojPos = BaselineLearner.makeNewLearner(
      baselineDataModel,
      baselineDataModel.wordFormConjNerConjPOS, baselineDataModel.goldLabel,
      "baselineLearnerWordFormConjNerCojPos-" + classifierModel, loadModelType
    )
    val baselineLearnerLemmaPair = BaselineLearner.makeNewLearner(
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
