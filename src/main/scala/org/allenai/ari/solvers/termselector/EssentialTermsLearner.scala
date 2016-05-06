package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.classifier.Learnable

/** A parameterized abstract class for learners for essential terms detection. */
abstract class EssentialTermsLearner(
    essentialTermsDataModel: EssentialTermsDataModel
) extends Learnable[Constituent](essentialTermsDataModel) {

  /** This allows access to sub-classes of EssentialTermsDataModel if set appropriately by
    * inheriting classes.
    */
  def dataModel: EssentialTermsDataModel

  /** Short name for the learner */
  def getSimpleName: String = getClass.getSimpleName

  /** Predict whether a given term is essential. */
  def predictIsEssential(c: Constituent): Boolean = {
    classifier.discreteValue(c) == EssentialTermsConstants.IMPORTANT_LABEL
  }

  /** Predict the probability of a given term being essential. */
  def predictProbOfBeingEssential(c: Constituent): Double = classifier.realValue(c)
}
