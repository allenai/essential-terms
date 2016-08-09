package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.params.LearnerParams
import org.allenai.ari.solvers.termselector.{ Sensors, Models }
import org.allenai.common.Logging
import edu.illinois.cs.cogcomp.lbjava.learn._

/** An expanded learner with a number of syntactic and semantic features. */
class ExpandedLearner(
    expandedDataModel: ExpandedDataModel,
    classifierModel: String
) extends IllinoisLearner(expandedDataModel, expandedDataModel.sensors) {

  // implement for trait EssentialTermsLearner
  // The reason has to have with def has to do with mutable objects in Saul (`*Learner` and `*DataModel` classes we extend)
  // In particular, many fields of `expandedDataModel` will get populated *afterwards* by that library once the training/test
  // data is processed.  One such field is `expandedDataModel.allProperties`, which we use below in
  // `override def features = dataModel.allProperties`. So, if we use `override val dataModel = ...` here, `features` will
  // have access to a stale snapshot of `expandedDataModel` whose field `allProperties` is null.
  override def dataModel = expandedDataModel

  // implement for trait Learnable[Constituent]
  override def label = classifier match {
    case _: StochasticGradientDescent => dataModel.goldRealConfidence
    case _ => dataModel.goldLabel
  }

  override lazy val classifier = classifierModel match {
    case "SVM" | "SVM-directAnswer" => new SupportVectorMachine()
    case "NaiveBayes" => new NaiveBayes()
    case "SparseNetwork" => new SparseNetworkLearner()
    case "Regression" => new StochasticGradientDescent()
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
    sensors: Sensors,
    learnerParams: LearnerParams,
    loadModelType: LoadType,
    baselineLearners: BaselineLearners,
    baselineDataModel: BaselineDataModel,
    salienceLearners: SalienceLearners
  ): (ExpandedDataModel, ExpandedLearner) = {
    val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners, salienceLearners, sensors)
    val expandedLearner = new ExpandedLearner(expandedDataModel, learnerParams.classifierModel)
    val models = new Models(learnerParams)
    models.load(expandedLearner, learnerParams.classifierModel, loadModelType)
    (expandedDataModel, expandedLearner)
  }
}
