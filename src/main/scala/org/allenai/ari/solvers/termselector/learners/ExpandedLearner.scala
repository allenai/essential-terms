package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.EssentialTermsSensors
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.lbjava.learn._

import java.io.File

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
}

object ExpandedLearner extends Logging {

  /** Make a new ExpandedLearner; also return the underlying BaselineLearner and data models.
    *
    * @param loadModelType load the pre-trained model, or the one on disk, or don't load any model
    */
  def makeNewLearner(
    loadModelType: String,
    classifierModel: String,
    baselineLearners: BaselineLearners,
    baselineDataModel: BaselineDataModel,
    salienceLearners: SalienceLearners
  ): (ExpandedDataModel, ExpandedLearner) = {
    val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners, salienceLearners)
    val expandedLearner = new ExpandedLearner(expandedDataModel, classifierModel)
    expandedLearner.modelSuffix = classifierModel
    loadModelType match {
      case "loadPreTrained" =>
        expandedLearner.modelDir = EssentialTermsSensors.preTrainedModels.toString + File.separator
        logger.debug(s"Loading baseline classifier from the pre-trained models ")
        expandedLearner.load()
      case "loadFromDisk" =>
        logger.debug(s"Loading ExpandedLearner model from ${expandedLearner.lcFilePath}")
        expandedLearner.load()
      case _ => // do nothing
    }
    (expandedDataModel, expandedLearner)
  }
}
