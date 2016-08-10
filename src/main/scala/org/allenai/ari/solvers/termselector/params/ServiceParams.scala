package org.allenai.ari.solvers.termselector.params

import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.config.{ ConfigFactory, Config }

import scala.collection.JavaConverters._

/** the set of all parameters necessary and sufficient for creating the ET service, which involves loading the annotations,
  * cached annotation, and some configurations related to annotation
  * @param stopwordsDatastoreFile
  * @param filterMidScoreConsitutents
  * @param scienceTermsDatastoreFile
  * @param regentsTrainingQuestion
  * @param checkForMissingSalienceScores
  * @param turkerEssentialityScores
  * @param combineNamedEntities
  * @param useRedisCachingForAnnotation whether to cache the output scores in a redis cache; would require you to run redis upon using
  */
class ServiceParams @Inject() (
    @Named("stopwordsDatastoreFile") val stopwordsDatastoreFile: String,
    @Named("annotation.filterMidScoreConsitutents") val filterMidScoreConsitutents: List[Double],
    @Named("scienceTermsDatastoreFile") val scienceTermsDatastoreFile: String,
    @Named("regentsTrainingQuestion") val regentsTrainingQuestion: String,
    @Named("annotation.checkForMissingSalienceScores") val checkForMissingSalienceScores: Boolean,
    @Named("turkerEssentialityScores") val turkerEssentialityScores: String,
    @Named("annotation.combineNamedEntities") val combineNamedEntities: Boolean,
    @Named("useRedisCachingForAnnotation") val useRedisCachingForAnnotation: Boolean
) {
  // nothing
}

object ServiceParams {
  def fromConfig(config: Config): ServiceParams = {
    new ServiceParams(
      config.getString("stopwordsDatastoreFile"),
      config.getDoubleList("annotation.filterMidScoreConsitutents").asScala.map(_.doubleValue()).toList,
      config.getString("scienceTermsDatastoreFile"),
      config.getString("regentsTrainingQuestion"),
      config.getBoolean("annotation.checkForMissingSalienceScores"),
      config.getString("turkerEssentialityScores"),
      config.getBoolean("annotation.combineNamedEntities"),
      config.getBoolean("useRedisCachingForAnnotation")
    )
  }

  val default: ServiceParams = {
    val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
    val localConfig = rootConfig.getConfig("ari.solvers.termselector")
    fromConfig(localConfig)
  }
}
