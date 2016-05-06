package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question

/** A generic trait for an essential terms learner for Aristo questions. */
trait GenericLearner {
  /** Get essential term scores for a given question. */
  def getEssentialTermScores(aristoQuestion: Question): Map[String, Double]

  /** Get essential terms for a given question. */
  def getEssentialTerms(aristoQuestion: Question): Seq[String]
}
