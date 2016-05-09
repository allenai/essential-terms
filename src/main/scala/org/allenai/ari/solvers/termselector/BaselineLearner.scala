package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.common.Logging

/** A baseline learner based on simply counting the label frequency per word */
class BaselineLearner(
    baselineDataModel: BaselineDataModel
) extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

  // implement for trait EssentialTermsLearner
  override def dataModel = baselineDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel
  override lazy val classifier = new CountClassifier

  override def feature = using(List(baselineDataModel.wordForm))
  override val logging = true
}

object BaselineLearner extends Logging {

  /** Make a new BaselineLearner. Also return the underlying data model.
    *
    * @param loadSavedModel whether to load a previously saved model
    */
  def makeNewLearner(loadSavedModel: Boolean): (BaselineDataModel, BaselineLearner) = {
    // create the baseline data model and the corresponding learner object
    val baselineDataModel = new BaselineDataModel
    val baselineLearner = new BaselineLearner(baselineDataModel)
    if (loadSavedModel) {
      logger.debug(s"Loading BaselineLearner model from ${baselineLearner.lcFilePath()}")
      baselineLearner.load()
    }
    (baselineDataModel, baselineLearner)
  }
}
