package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property
import org.allenai.ari.models.Question
import org.allenai.common.Logging

/** A baseline learner based on simply counting the label frequency per word
  * @param baselineDataModel
  * @param inputFeature what can be used to make the prediction; usually [[baselineDataModel.goldLabel]]
  * @param output what needs to be predicted; usually [[baselineDataModel.wordForm]]
  */
class BaselineLearner(
    baselineDataModel: BaselineDataModel,
    inputFeature: Property[Constituent],
    output: Property[Constituent]
) extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

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
    val baselineLearnerSurfaceForm = new BaselineLearner(baselineDataModel, baselineDataModel.wordForm, baselineDataModel.goldLabel)
    baselineLearnerSurfaceForm.modelSuffix = "surfaceForm"
    val baselineLearnerLemma = new BaselineLearner(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel)
    baselineLearnerLemma.modelSuffix = "lemma"
    val baselineLearnerPosConjLemma = new BaselineLearner(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel)
    baselineLearnerPosConjLemma.modelSuffix = "PosConjLemma"
    val baselineLearnerWordFormConjNer = new BaselineLearner(baselineDataModel, baselineDataModel.wordFormConjNer, baselineDataModel.goldLabel)
    baselineLearnerWordFormConjNer.modelSuffix = "baselineLearnerWordFormConjNer"
    val baselineLearnerWordFormConjNerCojPos = new BaselineLearner(baselineDataModel, baselineDataModel.wordFormConjNerConjPOS, baselineDataModel.goldLabel)
    baselineLearnerWordFormConjNerCojPos.modelSuffix = "baselineLearnerWordFormConjNerCojPos"
    val baselineLearnerLemmaPair = new BaselineLearner(baselineDataModel, baselineDataModel.lemmaPair, baselineDataModel.goldLabelPair)
    baselineLearnerLemmaPair.modelSuffix = "baselineLearnerLemmaPair"
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
      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerLemmaPair.lcFilePath}")
      baselineLearnerLemmaPair.load()
    }
    (baselineDataModel, BaselineLearners(baselineLearnerSurfaceForm, baselineLearnerLemma, baselineLearnerPosConjLemma,
      baselineLearnerWordFormConjNer, baselineLearnerWordFormConjNerCojPos, baselineLearnerLemmaPair))
  }
}

case class BaselineLearners(
  surfaceForm: BaselineLearner,
  lemma: BaselineLearner,
  posConjLemma: BaselineLearner,
  wordFormConjNer: BaselineLearner,
  wordFormConjNerConjPos: BaselineLearner,
  baselineLearnerLemmaPair: BaselineLearner
)
