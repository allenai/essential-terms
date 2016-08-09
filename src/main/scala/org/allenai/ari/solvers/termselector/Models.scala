package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.learners.{
  IllinoisLearner,
  LoadFromDatastore,
  LoadFromDisk,
  LoadType
}
import org.allenai.ari.solvers.termselector.params.LearnerParams
import org.allenai.common.Logging

import java.io.File

/** a small object keeping the variables and methods related to handling the models */
class Models(learnerParams: LearnerParams) extends Logging {
  //  models trained on the annotatetd data and saved in the datastore
  private lazy val preTrainedModels = learnerParams.modelsDatastoreFolder

  /** This method loads the proper model for a classifier
    *
    * @param illinoisLearner an input classifier
    * @param loadModelType whether to load from disk, from datastore, or nothing (firnafresh empty model)
    */
  def load(
    illinoisLearner: IllinoisLearner,
    loadModelType: LoadType
  ): Unit = {
    illinoisLearner.modelSuffix = learnerParams.classifierModel
    loadModelType match {
      case LoadFromDatastore =>
        illinoisLearner.modelDir = preTrainedModels.toString + File.separator
        logger.debug(s"Loading expanded classifier from the pre-trained models from datastore. ")
        illinoisLearner.load()
      case LoadFromDisk =>
        logger.debug(s"Loading ExpandedLearner model from ${illinoisLearner.lcFilePath}")
        illinoisLearner.load()
      case _ => logger.trace("Not loading any model . . .")
    }
  }
}
