package org.allenai.ari.solvers.termselector.params

import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.config.{ ConfigFactory, Config }

/** A set of parameters sufficient to create a learner
  * @param classifierType whether and how to identify and use essential terms in the model
  * @param classifierModel the type of the underlying model used for predictions (e.g. SVM, etc)
  * @param directAnswerQuestions
  */
class LearnerParams @Inject() (
    @Named("classifierType") val classifierType: String,
    @Named("classifierModel") val classifierModel: String,
    @Named("directAnswerQuestions") val directAnswerQuestions: Boolean,
    @Named("modelsDatastoreFolder") val modelsDatastoreFolder: String
) {
  // nothing
}

object LearnerParams {
  def fromConfig(config: Config): LearnerParams = {
    new LearnerParams(
      config.getString("classifierType"),
      config.getString("classifierModel"),
      config.getBoolean("directAnswerQuestions"),
      config.getString("modelsDatastoreFolder")
    )
  }

  val default: LearnerParams = {
    val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
    val localConfig = rootConfig.getConfig("ari.solvers.termselector")
    fromConfig(localConfig)
  }
}