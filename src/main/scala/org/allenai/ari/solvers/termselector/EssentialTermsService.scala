package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.common.Logging

/** A service for identifying essential terms in Aristo questions. */
class EssentialTermsService(
    learnerType: String,
    confidenceThreosholdOpt: Option[Double]
) extends Logging {

  logger.info(s"Initializing essential terms service with learner type: $learnerType")

  /** Create a learner object. */
  private val learner: GenericLearner = learnerType match {
    case "Lookup" => new LookupLearner(None)
    case "Baseline" => BaselineLearner.makeNewLearner(loadSavedModel = true)._2
    case "Expanded" => ExpandedLearner.makeNewLearner(loadSavedModel = true)._4
    case _ => throw new IllegalArgumentException(s"Unidentified learner type $learnerType")
  }

  /** Get essential term scores for a given question. */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    learner.getEssentialTermScores(aristoQ)
  }

  /** Get essential terms for a given question; use confidenceThreshold if provided. */
  def getEssentialTerms(aristoQ: Question): Seq[String] = {
    confidenceThreosholdOpt match {
      case Some(confidenceThreshold) =>
        val termsWithScores = getEssentialTermScores(aristoQ)
        termsWithScores.collect { case (term, score) if score >= confidenceThreshold => term }.toSeq
      case None =>
        learner.getEssentialTerms(aristoQ)
    }
  }
}
