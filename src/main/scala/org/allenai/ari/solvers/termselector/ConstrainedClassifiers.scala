package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Sentence, Constituent }
import edu.illinois.cs.cogcomp.lbjava.infer.OJalgoHook
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import edu.illinois.cs.cogcomp.saul.constraint.ConstraintTypeConversion._

import scala.collection.JavaConverters._

class ConstrainedClassifiers(
    expandedDataModel: ExpandedDataModel,
    learner: ExpandedLearner
) extends ConstrainedClassifier[Constituent, Sentence](learner) {
  def subjectTo = essentialTermsConstraints
  override val pathToHead = Some(expandedDataModel.tokenToSentence)
  override val solver = new OJalgoHook

  // constraints
  val essentialTermsConstraints = ConstrainedClassifier.constraint[Sentence] { x: Sentence =>
    // trivial constraint: all words should be essential
    x.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala.toSeq._forall { c =>
      (learner on c).is(EssentialTermsConstants.IMPORTANT_LABEL)
    }
  }
}
