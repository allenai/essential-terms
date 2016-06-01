package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.common.Logging

import com.google.inject.Inject
import com.google.inject.name.Named

/** A service for identifying essential terms in Aristo questions.
  * @param classifierType whether and how to identify and use essential terms in the model
  * @param confidenceThreshold Threshold to call terms essential. If set to a negative value, use
  * the classifier predictions directly
  */
class EssentialTermsService @Inject() (
    @Named("essentialTerms.classifierType") val classifierType: String,
    @Named("essentialTerms.confidenceThreshold") val confidenceThreshold: Double
) extends Logging {

  /** Create a learner object. Lazy to avoid creating a learner if the service is not used. */
  private lazy val learner: EssentialTermsLearner = {
    logger.info(s"Initializing essential terms service with learner type: $classifierType")
    classifierType match {
      case "Lookup" => new LookupLearner(None)
      case "Baseline" => BaselineLearner.makeNewLearners(loadSavedModel = true)._2.surfaceForm
      case "Expanded" => ExpandedLearner.makeNewLearner(loadSavedModel = true)._4
      case _ => throw new IllegalArgumentException(s"Unidentified learner type $classifierType")
    }
  }

  /** Get essential term scores for a given question. */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    val illinoisLearner = learner match {
      case x: IllinoisLearner => x
      case _ => throw new Exception("incompatible classifier type ")
    }
    EssentialTermsSensors.getEssentialTermProbForAristoQuestion(aristoQ, illinoisLearner)
  }

  /** Get essential terms for a given question; use confidenceThreshold if provided. */
  def getEssentialTerms(aristoQ: Question): Seq[String] = {
    confidenceThreshold match {
      case threshold if threshold >= 0 =>
        val termsWithScores = getEssentialTermScores(aristoQ)
        termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
      case _ =>
        learner match {
          case x: IllinoisLearner => EssentialTermsSensors.getEssentialTermsForAristoQuestion(aristoQ, x)
          case x: LookupLearner => x.getEssentialTerms(aristoQ)
          case _ => throw new Exception("incompatible classifier type ")
        }
    }
  }

  /** Get essential terms for a given question (selected via confidenceThreshold, if provided), as well as essential
    * term scores for a given question.
    */
  def getEssentialTermsAndScores(aristoQ: Question): (Seq[String], Map[String, Double]) = {
    val termsWithScores = getEssentialTermScores(aristoQ)
    val essentialTerms = confidenceThreshold match {
      case threshold if threshold >= 0 =>
        termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
      case _ =>
        learner match {
          case x: IllinoisLearner => EssentialTermsSensors.getEssentialTermsForAristoQuestion(aristoQ, x)
          case x: LookupLearner => x.getEssentialTerms(aristoQ)
          case _ => throw new Exception("incompatible classifier type ")
        }
    }
    (essentialTerms, termsWithScores)
  }
}
