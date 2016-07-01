package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.Models
import org.allenai.common.Logging
import edu.illinois.cs.cogcomp.lbjava.learn._

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

  /** Make a new ExpandedLearner; also return the underlying data models.
    *
    * @param loadModelType load the pre-trained model from disk or from datastore, or the one on disk, or don't load any model
    */
  def makeNewLearner(
    loadModelType: LoadType,
    classifierModel: String,
    baselineLearners: BaselineLearners,
    baselineDataModel: BaselineDataModel,
    salienceLearners: SalienceLearners
  ): (ExpandedDataModel, ExpandedLearner) = {
    val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners, salienceLearners)
    val expandedLearner = new ExpandedLearner(expandedDataModel, classifierModel)
    Models.setModel(expandedLearner, classifierModel, loadModelType)
    (expandedDataModel, expandedLearner)
  }
}
