package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Sentence, Constituent }
import edu.illinois.cs.cogcomp.lbjava.infer.OJalgoHook
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import edu.illinois.cs.cogcomp.saul.constraint.ConstraintTypeConversion._
import org.allenai.common.Logging

import scala.collection.JavaConverters._

class ConstrainedLearner(
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

object ConstrainedLearner extends Logging {

  /** Make a new constrained version of [[ExpandedLearner]].
    * Note that for using this constrained classifier you definitely have to have the other models saved on disk.
    */
  def makeNewLearner(
    expandedLearner: ExpandedLearner,
    expandedDataModel: ExpandedDataModel
  ): ConstrainedClassifier[Constituent, Sentence] = {
    new ConstrainedLearner(expandedDataModel, expandedLearner)
  }
}

