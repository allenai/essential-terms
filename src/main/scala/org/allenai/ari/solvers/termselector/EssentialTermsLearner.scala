package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question

/** A generic trait for an essential terms learner for Aristo questions. */
trait EssentialTermsLearner {
  /** Short name for the learner */
  def getSimpleName: String = getClass.getSimpleName

  /** Get essential terms for a given question. */
  def getEssentialTerms(aristoQuestion: Question): Seq[String]
}
