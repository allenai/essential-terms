package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.learners.{
  IllinoisLearner,
  LoadFromDatastore,
  LoadFromDisk,
  LoadType
}
import org.allenai.common.Logging

import java.io.File

/** a small object keeping the variables and methods related to handling the models */
object Models extends Logging {
  //  models trained on the annotatetd data and saved in the datastore
  private lazy val preTrainedModels = Utils.getDatastoreDirectoryAsFolder(
    Sensors.localConfig.getString("modelsDatastoreFolder")
  )

  def setModel(
    illinoisLearner: IllinoisLearner,
    classifierModel: String,
    loadModelType: LoadType
  ): Unit = {
    illinoisLearner.modelSuffix = classifierModel
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
