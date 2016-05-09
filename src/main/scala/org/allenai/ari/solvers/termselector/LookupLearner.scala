package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.EssentialTermsUtils.Levenshtein

/** A learner that simply looks up essential terms as annotated in MTurk data, if available. */
class LookupLearner(confidenceThresholdOpt: Option[Double]) extends EssentialTermsLearner {

  private val DEFAULT_CONFIDENCE_THRESHOLD = 0.9
  private val confidenceThreshold = confidenceThresholdOpt.getOrElse(DEFAULT_CONFIDENCE_THRESHOLD)

  lazy val questionEssentialTermScores = EssentialTermsSensors.allQuestions.map { q =>
    q.rawQuestion -> q.essentialTermMap
  }

  /** Get MTurk annotated essential term scores for a given question. */
  def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    val (_, termMap, minDistance) = questionEssentialTermScores.map {
      case (q, termScores) => (q, termScores, Levenshtein.distance(q, aristoQuestion.rawQuestion))
    }.minBy { case (_, _, distance) => distance }
    val DISTANCE_THRESHOLD = 5
    if (minDistance < DISTANCE_THRESHOLD) {
      termMap.getOrElse(Map.empty)
    } else {
      throw new NoSuchElementException(s"Annotation not found for question: $aristoQuestion")
    }
  }

  /** Get MTurk annotated essential term scores for a given question. */
  def getEssentialTerms(aristoQuestion: Question): Seq[String] = {
    val termScores = getEssentialTermScores(aristoQuestion)
    termScores.collect { case (term, score) if score >= confidenceThreshold => term }.toSeq
  }
}
