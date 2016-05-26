package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.classifier.Learnable

/** A parameterized abstract class for UIUC learners for essential terms detection. */
abstract class IllinoisLearner(
    essentialTermsDataModel: IllinoisDataModel
) extends Learnable[Constituent](essentialTermsDataModel.tokens) with EssentialTermsLearner {

  /** This allows access to sub-classes of EssentialTermsDataModel if set appropriately by
    * inheriting classes.
    */
  def dataModel: IllinoisDataModel

  // implement for trait MyLearner
  def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    EssentialTermsSensors.getEssentialTermProbForAristoQuestion(aristoQuestion, this)
  }

  // implement for trait MyLearner
  def getEssentialTerms(aristoQuestion: Question): Seq[String] = {
    EssentialTermsSensors.getEssentialTermsForAristoQuestion(aristoQuestion, this)
  }

  /** Predict the class label of a given term. */
  def predictLabel(c: Constituent): String = classifier.discreteValue(c)

  /** Predict whether a given term is essential. */
  def predictIsEssential(c: Constituent): Boolean = {
    classifier.discreteValue(c) == EssentialTermsConstants.IMPORTANT_LABEL
  }

  /** Predict the probability of a given term being essential. */
  def predictProbOfBeingEssential(c: Constituent): Double = {
    classifier.scores(c).toArray.find {
      case score => score.value == EssentialTermsConstants.IMPORTANT_LABEL
    }.get.score
  }
}
