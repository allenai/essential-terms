package org.allenai.ari.solvers.termselector.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** the set of all parameters necessary and sufficient for creating the ET service, which involves loading the annotations,
  * cached annotation, and some configurations related to annotation
  * @param stopwordsDatastoreFile
  * @param filterMidScoreConsitutents
  * @param scienceTermsDatastoreFile
  * @param regentsTrainingQuestion
  * @param checkForMissingSalienceScores
  * @param turkerEssentialityScores
  * @param combineNamedEntities
  * @param useRedisCaching whether to cache the output scores in a redis cache; would require you to run redis upon using
  */
class ServiceParams @Inject() (
    @Named("stopwordsDatastoreFile") val stopwordsDatastoreFile: String,
    @Named("annotation.filterMidScoreConsitutents") val filterMidScoreConsitutents: List[Double],
    @Named("scienceTermsDatastoreFile") val scienceTermsDatastoreFile: String,
    @Named("regentsTrainingQuestion") val regentsTrainingQuestion: String,
    @Named("annotation.checkForMissingSalienceScores") val checkForMissingSalienceScores: Boolean,
    @Named("turkerEssentialityScores") val turkerEssentialityScores: String,
    @Named("annotation.combineNamedEntities") val combineNamedEntities: Boolean,
    @Named("useRedisCaching") val useRedisCaching: Boolean
) {
  // nothing
}
