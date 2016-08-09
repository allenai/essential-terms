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
  /** This method loads the proper model for a classifier
    * @param illinoisLearner an input classifier
    * @param classifierModel the type of the classifier (e.g. SVM, trained-on-dev, etc)
    * @param loadModelType whether to load from disk, from datastore, or nothing (firnafresh empty model)
    */
  def load(
    illinoisLearner: IllinoisLearner,
    classifierModel: String,
    loadModelType: LoadType
  ): Unit = {
    illinoisLearner.modelSuffix = classifierModel
    loadModelType match {
      case LoadFromDatastore =>
        //  models trained on the annotated data and saved in the datastore
        illinoisLearner.modelDir = Utils.getDatastoreDirectoryAsFolder(learnerParams.modelsDatastoreFolder).toString + File.separator
        logger.debug(s"Loading expanded classifier from the pre-trained models from datastore. ")
        illinoisLearner.load()
      case LoadFromDisk =>
        logger.debug(s"Loading ExpandedLearner model from ${illinoisLearner.lcFilePath}")
        illinoisLearner.load()
      case _ => logger.trace("Not loading any model . . .")
    }
  }
}
