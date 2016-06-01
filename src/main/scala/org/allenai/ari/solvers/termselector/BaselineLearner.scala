package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.classifier.ClassifierUtils
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property
import org.allenai.common.Logging

/** A baseline learner based on simply counting the label frequency per word
  *
  * @param baselineDataModel
  * @param input what can be used to make the prediction; usually [[baselineDataModel.wordForm]]
  * @param output what needs to be predicted; usually [[baselineDataModel.goldLabel]]
  * @param suffix suffix use when saving the models on disk
  */
class BaselineLearner(
    baselineDataModel: BaselineDataModel,
    input: Property[Constituent],
    output: Property[Constituent],
    suffix: String = ""
) extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

  // implement for trait EssentialTermsLearner
  override def dataModel = baselineDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel
  override lazy val classifier = new CountClassifier

  override def feature = using(input)
  override val logging = true
  override val modelSuffix = suffix
}

object BaselineLearner extends Logging {

  /** Make a new BaselineLearner. Also return the underlying data model.
    *
    * @param loadSavedModel whether to load a previously saved model
    */
  def makeNewLearners(loadSavedModel: Boolean): (BaselineDataModel, BaselineLearners) = {
    // create the baseline data model and the corresponding learner object
    // TODO: make the creation of the baselines simpler
    val baselineDataModel = new BaselineDataModel
    val baselineLearnerSurfaceForm = new BaselineLearner(baselineDataModel, baselineDataModel.wordForm, baselineDataModel.goldLabel, "surfaceForm")
    val baselineLearnerLemma = new BaselineLearner(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel, "lemma")
    val baselineLearnerPosConjLemma = new BaselineLearner(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel, "PosConjLemma")
    val baselineLearnerWordFormConjNer = new BaselineLearner(baselineDataModel, baselineDataModel.wordFormConjNer, baselineDataModel.goldLabel, "baselineLearnerWordFormConjNer")
    val baselineLearnerWordFormConjNerCojPos = new BaselineLearner(baselineDataModel, baselineDataModel.wordFormConjNerConjPOS, baselineDataModel.goldLabel, "baselineLearnerWordFormConjNerCojPos")
    val baselineLearnerLemmaPair = new BaselineLearner(baselineDataModel, baselineDataModel.lemmaPair, baselineDataModel.goldLabelPair, "baselineLearnerLemmaPair")
    if (loadSavedModel) {
      logger.debug(s"Loading baseline classifiers . . . ")
      ClassifierUtils.LoadClassifier(baselineLearnerSurfaceForm, baselineLearnerLemma, baselineLearnerPosConjLemma,
        baselineLearnerWordFormConjNer, baselineLearnerWordFormConjNerCojPos, baselineLearnerLemmaPair)
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
