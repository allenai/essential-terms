package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property
import org.allenai.ari.models.Question
import org.allenai.common.Logging

/** A baseline learner based on simply counting the label frequency per word */
class BaselineLearner(
  baselineDataModel: BaselineDataModel
)(inputFeature: Property[Constituent] = baselineDataModel.wordForm)
    extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

  // implement for trait EssentialTermsLearner
  override def dataModel = baselineDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel
  override lazy val classifier = new CountClassifier

  override def feature = using(inputFeature)
  override val logging = true
}

object BaselineLearner extends Logging {

  /** Make a new BaselineLearner. Also return the underlying data model.
    *
    * @param loadSavedModel whether to load a previously saved model
    */
  def makeNewLearners(loadSavedModel: Boolean): (BaselineDataModel, BaselineLearners) = {
    // create the baseline data model and the corresponding learner object
    val baselineDataModel = new BaselineDataModel
    val baselineLearnerSurfaceForm = new BaselineLearner(baselineDataModel)(baselineDataModel.wordForm)
    baselineLearnerSurfaceForm.modelSuffix = "surfaceForm"
    val baselineLearnerLemma = new BaselineLearner(baselineDataModel)(baselineDataModel.lemma)
    baselineLearnerLemma.modelSuffix = "lemma"
    val baselineLearnerPosConjLemma = new BaselineLearner(baselineDataModel)(baselineDataModel.lemma)
    baselineLearnerPosConjLemma.modelSuffix = "PosConjLemma"
    val baselineLearnerWordFormConjNer = new BaselineLearner(baselineDataModel)(baselineDataModel.wordFormConjNer)
    baselineLearnerWordFormConjNer.modelSuffix = "baselineLearnerWordFormConjNer"
    val baselineLearnerWordFormConjNerCojPos = new BaselineLearner(baselineDataModel)(baselineDataModel.wordFormConjNerConjPOS)
    baselineLearnerWordFormConjNerCojPos.modelSuffix = "baselineLearnerWordFormConjNerCojPos"
    if (loadSavedModel) {
      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerSurfaceForm.lcFilePath}")
      baselineLearnerSurfaceForm.load()
      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerLemma.lcFilePath}")
      baselineLearnerLemma.load()
      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerPosConjLemma.lcFilePath}")
      baselineLearnerPosConjLemma.load()
      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerWordFormConjNer.lcFilePath}")
      baselineLearnerWordFormConjNer.load()
      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerWordFormConjNerCojPos.lcFilePath}")
      baselineLearnerWordFormConjNerCojPos.load()
    }
    (baselineDataModel, BaselineLearners(baselineLearnerSurfaceForm, baselineLearnerLemma, baselineLearnerPosConjLemma,
      baselineLearnerWordFormConjNer, baselineLearnerWordFormConjNerCojPos))
  }
}

case class BaselineLearners(
  surfaceForm: BaselineLearner,
  lemma: BaselineLearner,
  posConjLemma: BaselineLearner,
  wordFormConjNer: BaselineLearner,
  wordFormConjNerConjPos: BaselineLearner
)
