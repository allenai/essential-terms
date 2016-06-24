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
      case "Lookup" => new LookupLearner() -> 0.5
      case "maxSalience" => SalienceLearner.makeNewLearners().max -> 0.02
      case "sumSalience" => SalienceLearner.makeNewLearners().sum -> 0.07
      case "Baseline" => BaselineLearner.makeNewLearners(loadSavedModel = true)._2.lemma -> 0.19
      case "Expanded" =>
        val salienceBaselines = SalienceLearner.makeNewLearners()
        val (baselineDataModel, baselineClassifiers) = BaselineLearner.makeNewLearners(loadSavedModel = true)
        ExpandedLearner.makeNewLearner(loadSavedModel = true, classifierModel, baselineClassifiers,
          baselineDataModel, salienceBaselines)._2 -> 0.46
      case _ => throw new IllegalArgumentException(s"Unidentified learner type $classifierType")
    }
  }

  /** Get essential term scores for a given question. */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    if (useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
  }

  /** Get essential terms for a given question; use threshold if provided, otherwise defaultThreshold */
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

  /** Retrieve essential scores from Redis cache; if not present, compute and store. */
  private def getEssentialScoresFromRedis(
    aristoQ: Question
  ): Map[String, Double] = {
    // use the raw question in the cache as the essential term prediction depends on the options
    val cacheKey = "EssentialTermsServiceCache***scores***" + aristoQ.rawQuestion + classifierType +
      classifierModel
    val termsAndScoreJsonOpt = EssentialTermsSensors.synchronized {
      EssentialTermsSensors.annotationRedisCache.get(cacheKey)
    }
    termsAndScoreJsonOpt match {
      case Some(termsAndScoreJson) =>
        termsAndScoreJson.parseJson.convertTo[Map[String, Double]]
      case None =>
        val scores = learner.getEssentialTermScores(aristoQ)
        EssentialTermsSensors.synchronized {
          EssentialTermsSensors.annotationRedisCache.set(
            cacheKey, scores.toJson.compactPrint
          )
        }
        scores
    }
  }
}
