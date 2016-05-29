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
  def makeNewLearner(loadSavedModel: Boolean): (BaselineDataModel, BaselineLearners) = {
    // create the baseline data model and the corresponding learner object
    val baselineDataModel = new BaselineDataModel
    val baselineLearnerSurfaceForm = new BaselineLearner(baselineDataModel)(baselineDataModel.wordForm)
    val baselineLearnerLemma = new BaselineLearner(baselineDataModel)(baselineDataModel.lemma)
    val baselineLearnerPosConjLemma = new BaselineLearner(baselineDataModel)(baselineDataModel.lemma)
    val baselineLearnerWordFormConjNer = new BaselineLearner(baselineDataModel)(baselineDataModel.wordFormConjNer)
    val baselineLearnerWordFormConjNerCojPos = new BaselineLearner(baselineDataModel)(baselineDataModel.wordFormConjNerConjPOS)
    if (loadSavedModel) {
      //      logger.debug(s"Loading BaselineLearner model from ${baselineLearnerLemma.lcFilePath()}")
      //      baselineLearnerLemma.load()
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
