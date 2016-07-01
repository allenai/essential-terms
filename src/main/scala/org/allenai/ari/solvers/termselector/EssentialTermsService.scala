package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.common.Logging

import com.google.inject.Inject
import com.google.inject.name.Named
import spray.json._
import DefaultJsonProtocol._

/** A service for identifying essential terms in Aristo questions.
  *
  * @param classifierType whether and how to identify and use essential terms in the model
  * @param classifierModel the type of the underlying model used for predictions
  * @param useRedisCaching whethet to cache the output scores in a redis cache; would require you to run redis upon using
  */
class EssentialTermsService @Inject() (
    @Named("essentialTerms.classifierType") val classifierType: String,
    @Named("essentialTerms.classifierModel") val classifierModel: String,
    @Named("essentialTerms.useRedisCaching") val useRedisCaching: Boolean
) extends Logging {

  /** Create a learner object. Lazy to avoid creating a learner if the service is not used.
    * The default thresholds are chosen to maximize F1 on the dev set, given the threshold
    */
  private lazy val (learner, defaultThreshold) = {
    logger.info(s"Initializing essential terms service with learner type: $classifierType")
    classifierType match {
      case "Lookup" => new LookupLearner() -> Constants.LOOKUP_THRESHOLD
      case "MaxSalience" => SalienceLearner.makeNewLearners().max -> Constants.MAX_SALIENCE_THRESHOLD
      case "SumSalience" => SalienceLearner.makeNewLearners().sum -> Constants.SUM_SALIENCE_THRESHOLD
      case "LemmaBaseline" => BaselineLearners.makeNewLearners(
        loadModelType = LoadFromDatastore, "train"
      )._2.lemma -> Constants.LEMMA_BASELINE_THRESHOLD
      case "Expanded" =>
        val salienceBaselines = SalienceLearner.makeNewLearners()
        val (baselineDataModel, baselineClassifiers) =
          BaselineLearners.makeNewLearners(LoadFromDatastore, "dev")
        ExpandedLearner.makeNewLearner(LoadFromDatastore, classifierModel, baselineClassifiers,
          baselineDataModel, salienceBaselines)._2 -> Constants.EXPANDED_LEARNER_THRESHOLD
      case _ => throw new IllegalArgumentException(s"Unidentified learner type $classifierType")
    }
  }

  /** Get essential term scores for a given question.
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @return a hashmap of the terms and their importance
    */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    if (useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
  }

  /** Get essential terms for a given question; use threshold if provided, otherwise defaultThreshold
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @param threshold the threshold above which a term is considered essential
    * @return a sequence of essential terms in the input questions
    */
  def getEssentialTerms(aristoQ: Question, threshold: Double = defaultThreshold): Seq[String] = {
    require(threshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = if (useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
    termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
  }

  /** Get essential terms for a given question (selected via threshold, if provided; otherwise defaultThreshold),
    * as well as essential term scores for a given question.
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @param threshold the threshold above which a term is considered essential
    * @return a tuple containing a sequence of essential terms as well as a hashmap of terms and their essentiality scores
    */
  def getEssentialTermsAndScores(aristoQ: Question, threshold: Double = defaultThreshold): (Seq[String], Map[String, Double]) = {
    require(threshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = if (useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
    val essentialTerms = termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
    (essentialTerms, termsWithScores)
  }

  /** Retrieve essential scores from Redis cache; if not present, compute and store.
    * @param aristoQ an input question in Aristo's standard question datastructure
    * @return a hashmap of the terms and their importance
    */
  private def getEssentialScoresFromRedis(
    aristoQ: Question
  ): Map[String, Double] = {
    // use the raw question in the cache as the essential term prediction depends on the options
    val cacheKey = "EssentialTermsServiceCache***scores***" + aristoQ.rawQuestion + classifierType +
      classifierModel
    val termsAndScoreJsonOpt = Sensors.synchronized {
      Annotator.synchronizedRedisClient.redisGet(cacheKey)
    }
    termsAndScoreJsonOpt match {
      case Some(termsAndScoreJson) =>
        termsAndScoreJson.parseJson.convertTo[Map[String, Double]]
      case None =>
        val scores = learner.getEssentialTermScores(aristoQ)
        Sensors.synchronized {
          Annotator.synchronizedRedisClient.redisSet(
            cacheKey, scores.toJson.compactPrint
          )
        }
        scores
    }
  }
}
