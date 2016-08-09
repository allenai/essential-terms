package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.params.LearnerParams
import org.allenai.ari.solvers.termselector.{ Constants, Models }
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property

/** A baseline learner based on simply counting the label frequency per word
  *
  * @param baselineDataModel the class which contains all the information relating the input and output variables
  * @param input what can be used to make the prediction; usually [[baselineDataModel.wordForm]]
  * @param output what needs to be predicted; usually [[baselineDataModel.goldLabel]]
  */
class BaselineLearner(
    baselineDataModel: BaselineDataModel,
    input: Property[Constituent],
    output: Property[Constituent]
) extends IllinoisLearner(baselineDataModel, baselineDataModel.sensors) with EssentialTermsLearner {

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
  /** Creates a single [[BaselineLearner]].
    *
    * @param baselineDataModel TODO(daniel) add description
    * @param input
    * @param output
    * @param suffix suffix to use when saving the models on disk
    * @param loadSavedModel
    * @return a baseline learner
    */
  def makeNewLearner(
    learnerParams: LearnerParams,
    baselineDataModel: BaselineDataModel,
    input: Property[Constituent],
    output: Property[Constituent],
    suffix: String,
    loadSavedModel: LoadType
  ): BaselineLearner = {
    val baseline = new BaselineLearner(baselineDataModel, input, output)
    val models = new Models(learnerParams)
    models.load(baseline, loadSavedModel)
    baseline
  }
}
