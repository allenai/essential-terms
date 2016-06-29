package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.{ Constants, EssentialTermsSensors }
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property

import java.io.File

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

  override def predictProbOfBeingEssential(c: Constituent): Double = {
    val scores = classifier.scores(c).toArray
    scores.collectFirst {
      case score if score.value == Constants.IMPORTANT_LABEL => score.score
    }.getOrElse(0.0) // if we have not seen the word in the training data it is never important
  }
}

object BaselineLearner extends Logging {

  /** Make a collection of [[BaselineLearner]]s. Also return the underlying data model.
    *
    * @param loadModelType load the pre-trained model, or the one on disk, or don't load any model
    */
  def makeNewLearners(loadModelType: LoadType, classifierModel: String): (BaselineDataModel, BaselineLearners) = {
    // the baselines are trained either on train or test
    require(classifierModel == "dev" || classifierModel == "train")
    // create the baseline data model and the corresponding learner object
    // TODO: make the creation of the baselines simpler
    val baselineDataModel = new BaselineDataModel
    val baselineLearnerSurfaceForm = createASingleBaseline(baselineDataModel, baselineDataModel.wordForm, baselineDataModel.goldLabel, "surfaceForm-" + classifierModel, loadModelType)
    val baselineLearnerLemma = createASingleBaseline(baselineDataModel, baselineDataModel.lemma, baselineDataModel.goldLabel, "lemma-" + classifierModel, loadModelType)
    val baselineLearnerPosConjLemma = createASingleBaseline(baselineDataModel, baselineDataModel.posConjLemma, baselineDataModel.goldLabel, "PosConjLemma-" + classifierModel, loadModelType)
    val baselineLearnerWordFormConjNer = createASingleBaseline(baselineDataModel, baselineDataModel.wordFormConjNer, baselineDataModel.goldLabel, "baselineLearnerWordFormConjNer-" + classifierModel, loadModelType)
    val baselineLearnerWordFormConjNerCojPos = createASingleBaseline(baselineDataModel, baselineDataModel.wordFormConjNerConjPOS, baselineDataModel.goldLabel, "baselineLearnerWordFormConjNerCojPos-" + classifierModel, loadModelType)
    val baselineLearnerLemmaPair = createASingleBaseline(baselineDataModel, baselineDataModel.lemmaPair, baselineDataModel.goldLabelPair, "baselineLearnerLemmaPair-" + classifierModel, loadModelType)
    (baselineDataModel, BaselineLearners(baselineLearnerSurfaceForm, baselineLearnerLemma, baselineLearnerPosConjLemma,
      baselineLearnerWordFormConjNer, baselineLearnerWordFormConjNerCojPos, baselineLearnerLemmaPair))
  }

  /** This creates a single [[BaselineLearner]].
    *
    * @param suffix suffix use when saving the models on disk
    * @return
    */
  def createASingleBaseline(baselineDataModel: BaselineDataModel, input: Property[Constituent],
    output: Property[Constituent], suffix: String, loadSavedModel: LoadType): BaselineLearner = {
    val baseline = new BaselineLearner(baselineDataModel, input, output)
    baseline.modelSuffix = suffix
    loadSavedModel match {
      case LoadFromDatastore =>
        baseline.modelDir = EssentialTermsSensors.preTrainedModels.toString + File.separator
        logger.info(s"Loading baseline classifier from the pre-trained models ")
        baseline.load()
      case LoadFromDisk =>
        logger.info(s"Loading baseline classifier ${baseline.lcFilePath} ")
        baseline.load()
      case _ =>
        logger.trace("Not loading any model . . .")
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
