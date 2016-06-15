package org.allenai.ari.solvers.termselector

import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn._
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property

/** An expanded learner with a number of syntactic and semantic features. */
class ExpandedLearner(
    expandedDataModel: ExpandedDataModel,
    classifierModel: String
) extends IllinoisLearner(expandedDataModel) {

  // implement for trait EssentialTermsLearner
  override def dataModel = expandedDataModel

  // implement for trait Learnable[Constituent]
  override def label = if (classifier.isInstanceOf[StochasticGradientDescent]) dataModel.goldRealConfidence else dataModel.goldLabel

  override lazy val classifier = classifierModel match {
    case "SVM" => new SupportVectorMachine()
    case "NaiveBayes" => new NaiveBayes()
    case "SparseNetwork" => new SparseNetworkLearner()
    case "regression" => new StochasticGradientDescent()
    case _ => throw new Exception("Wrong classifier type or not specified ")
  }

  override def feature = dataModel.allProperties
  override val logging = true
}

object ExpandedLearner extends Logging {

  /** Make a new ExpandedLearner; also return the underlying BaselineLearner and data models.
    *
    * @param loadSavedModel whether to load a previously saved model
    */
  def makeNewLearner(
    loadSavedModel: Boolean,
    classifierModel: String,
    baselineLearners: BaselineLearners,
    baselineDataModel: BaselineDataModel,
    salienceLearners: SalienceBaselines
  ): (ExpandedDataModel, ExpandedLearner) = {
    val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners, salienceLearners)
    val expandedLearner = new ExpandedLearner(expandedDataModel, classifierModel)
    expandedLearner.modelSuffix = classifierModel
    if (loadSavedModel) {
      logger.debug(s"Loading ExpandedLearner model from ${expandedLearner.lcFilePath}")
      expandedLearner.load()
    }
    (expandedDataModel, expandedLearner)
  }
}
