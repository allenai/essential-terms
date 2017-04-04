package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.datamodel.Question
import org.allenai.ari.solvers.termselector.Sensors
import org.allenai.ari.solvers.termselector.Utils.Levenshtein

/** A learner that simply looks up essential terms as annotated in MTurk data, if available. */
class LookupLearner(sensors: Sensors) extends EssentialTermsLearner {

  lazy val questionEssentialTermScores = sensors.allQuestions.map { q =>
    q.aristoQuestion.text.getOrElse(q.aristoQuestion.rawQuestion) -> q.essentialTermMap
  }

  /** Get MTurk annotated essential term scores for a given question. */
  def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    val (_, termMap, minDistance) = questionEssentialTermScores.map {
      case (q, termScores) => (q, termScores, Levenshtein.distance(q, aristoQuestion.text
        .getOrElse(aristoQuestion.rawQuestion)))
    }.minBy { case (_, _, distance) => distance }
    val DISTANCE_THRESHOLD = 5
    if (minDistance < DISTANCE_THRESHOLD) {
      termMap.getOrElse(Map.empty)
    } else {
      throw new NoSuchElementException(s"Annotation not found for question: $aristoQuestion")
    }
  }

  /** Get MTurk annotated essential term scores for a given question. */
  def getEssentialTerms(aristoQuestion: Question, confidenceThreshold: Double): Seq[String] = {
    val termScores = getEssentialTermScores(aristoQuestion)
    termScores.collect { case (term, score) if score >= confidenceThreshold => term }.toSeq
  }
}
