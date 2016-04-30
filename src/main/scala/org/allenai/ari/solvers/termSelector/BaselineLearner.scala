package org.allenai.ari.solvers.termselector

/** A baseline learner based on simply counting the label frequency per word */
class BaselineLearner(
    baselineDataModel: BaselineDataModel
) extends EssentialTermsLearner(baselineDataModel) {

  // implement for trait EssentialTermsLearner
  override def dataModel = baselineDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel
  override lazy val classifier = new CountClassifier

  override def feature = using(List(baselineDataModel.wordForm))
  override val loggging = true
}

