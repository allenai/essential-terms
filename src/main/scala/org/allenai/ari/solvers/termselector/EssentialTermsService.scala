package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.common.Logging

import com.google.inject.Inject
import com.google.inject.name.Named
import spray.json._
import DefaultJsonProtocol._

/** A service for identifying essential terms in Aristo questions.
  *
  * @param classifierType whether and how to identify and use essential terms in the model
  * @param confidenceThreshold Threshold to call terms essential. If set to a negative value, use
  * the classifier predictions directly
  */
class EssentialTermsService @Inject() (
    @Named("essentialTerms.classifierType") val classifierType: String,
    @Named("essentialTerms.confidenceThreshold") val confidenceThreshold: Double,
    @Named("essentialTerms.classifierModel") val classifierModel: String,
    @Named("essentialTerms.useRedisCaching") val useRedisCaching: Boolean
) extends Logging {

  /** Create a learner object. Lazy to avoid creating a learner if the service is not used. */
  private lazy val learner: EssentialTermsLearner = {
    logger.info(s"Initializing essential terms service with learner type: $classifierType")
    classifierType match {
      case "Lookup" => new LookupLearner()
      case "maxSalience" => SalienceBaseline.makeNewLearners().max
      case "sumSalience" => SalienceBaseline.makeNewLearners().sum
      case "Baseline" => BaselineLearner.makeNewLearners(loadSavedModel = true)._2.surfaceForm
      case "Expanded" =>
        val salienceBaselines = SalienceBaseline.makeNewLearners()
        val (baselineDataModel, baselineClassifiers) = BaselineLearner.makeNewLearners(loadSavedModel = true)
        ExpandedLearner.makeNewLearner(loadSavedModel = true, classifierModel, baselineClassifiers,
          baselineDataModel, salienceBaselines)._2
      case _ => throw new IllegalArgumentException(s"Unidentified learner type $classifierType")
    }
  }

  /** Get essential term scores for a given question. */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    if (useRedisCaching) {
      getEssentialTermsAndScoresFromRedis(aristoQ)._2
    } else {
      computeEssentialTermScores(aristoQ)
    }
  }

  /** Get essential terms for a given question; use confidenceThreshold if provided. */
  def getEssentialTerms(aristoQ: Question): Seq[String] = {
    if (useRedisCaching) {
      getEssentialTermsAndScoresFromRedis(aristoQ)._1
    } else {
      computeEssentialTerms(aristoQ)
    }
  }

  /** Get essential terms for a given question (selected via confidenceThreshold, if provided),
    * as well as essential term scores for a given question.
    */
  def getEssentialTermsAndScores(aristoQ: Question): (Seq[String], Map[String, Double]) = {
    if (useRedisCaching) {
      getEssentialTermsAndScoresFromRedis(aristoQ)
    } else {
      computeEssentialTermsAndScores(aristoQ)
    }
  }

  /** Compute essential term scores for a given question. */
  private def computeEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    learner.getEssentialTermScores(aristoQ)
  }

  /** Compute essential terms for a given question; use confidenceThreshold if provided. */
  private def computeEssentialTerms(aristoQ: Question): Seq[String] = {
    require(confidenceThreshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = computeEssentialTermScores(aristoQ)
    termsWithScores.collect { case (term, score) if score >= confidenceThreshold => term }.toSeq
  }

  /** Compute essential terms for a given question (selected via confidenceThreshold, if provided),
    * as well as essential term scores for a given question.
    */
  private def computeEssentialTermsAndScores(
    aristoQ: Question
  ): (Seq[String], Map[String, Double]) = {
    require(confidenceThreshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = computeEssentialTermScores(aristoQ)
    val essentialTerms = termsWithScores.collect { case (term, score) if score >= confidenceThreshold => term }.toSeq

    (essentialTerms, termsWithScores)
  }

  /** Retrieve essential terms and scores from Redis cache; if not present, compute and store. */
  private def getEssentialTermsAndScoresFromRedis(
    aristoQ: Question
  ): (Seq[String], Map[String, Double]) = {
    // use the raw question in the cache as the essential term prediction depends on the options
    val cacheKey = "EssentialTermsServiceCache***" + aristoQ.rawQuestion + classifierType +
      classifierModel
    val termsAndScoreJsonOpt = EssentialTermsSensors.synchronized {
      EssentialTermsSensors.annotationRedisCache.get(cacheKey)
    }
    termsAndScoreJsonOpt match {
      case Some(termsAndScoreJson) =>
        termsAndScoreJson.parseJson.convertTo[(Seq[String], Map[String, Double])]
      case None =>
        val (terms, scores) = computeEssentialTermsAndScores(aristoQ)
        EssentialTermsSensors.synchronized {
          EssentialTermsSensors.annotationRedisCache.set(
            cacheKey, (terms, scores).toJson.compactPrint
          )
        }
        (terms, scores)
    }
  }
}
