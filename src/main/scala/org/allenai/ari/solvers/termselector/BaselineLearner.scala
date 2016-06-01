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
  */
class BaselineLearner(
    baselineDataModel: BaselineDataModel,
    input: Property[Constituent],
    output: Property[Constituent]
) extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

  // implement for trait EssentialTermsLearner
  override def dataModel = baselineDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel
  override lazy val classifier = new CountClassifier

  override def feature = using(input)
  override val logging = true
}

object BaselineLearner extends Logging {

  /** Make a collection of [[BaselineLearner]]s. Also return the underlying data model.
    *
    * @param loadSavedModel whether to load a previously saved model
    */
  def makeNewLearners(loadSavedModel: Boolean): (BaselineDataModel, BaselineLearners) = {
    // create the baseline data model and the corresponding learner object
    // TODO: make the creation of the baselines simpler
    val baselineDataModel = new BaselineDataModel
    val baselineLearnerSurfaceForm = createASingleBaseline(baselineDataModel, baselineDataModel.wordForm, baselineDataModel.goldLabel, "surfaceForm", loadSavedModel)
    val baselineLearnerLemma = createASingleBaseline(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel, "lemma", loadSavedModel)
    val baselineLearnerPosConjLemma = createASingleBaseline(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel, "PosConjLemma", loadSavedModel)
    val baselineLearnerWordFormConjNer = createASingleBaseline(baselineDataModel, baselineDataModel.wordFormConjNer, baselineDataModel.goldLabel, "baselineLearnerWordFormConjNer", loadSavedModel)
    val baselineLearnerWordFormConjNerCojPos = createASingleBaseline(baselineDataModel, baselineDataModel.wordFormConjNerConjPOS, baselineDataModel.goldLabel, "baselineLearnerWordFormConjNerCojPos", loadSavedModel)
    val baselineLearnerLemmaPair = createASingleBaseline(baselineDataModel, baselineDataModel.lemmaPair, baselineDataModel.goldLabelPair, "baselineLearnerLemmaPair", loadSavedModel)
    (baselineDataModel, BaselineLearners(baselineLearnerSurfaceForm, baselineLearnerLemma, baselineLearnerPosConjLemma,
      baselineLearnerWordFormConjNer, baselineLearnerWordFormConjNerCojPos, baselineLearnerLemmaPair))
  }

  /** This creates a single [[BaselineLearner]].
    * @param suffix suffix use when saving the models on disk
    * @return
    */
  def createASingleBaseline(baselineDataModel: BaselineDataModel, input: Property[Constituent],
    output: Property[Constituent], suffix: String, loadSavedModel: Boolean): BaselineLearner = {
    val baseline = new BaselineLearner(baselineDataModel, baselineDataModel.wordForm, baselineDataModel.goldLabel)
    baseline.modelSuffix = suffix
    if (loadSavedModel) {
      logger.debug(s"Loading baseline classifier ${baseline.getSimpleName} ")
      baseline.load()
    }
    baseline
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
