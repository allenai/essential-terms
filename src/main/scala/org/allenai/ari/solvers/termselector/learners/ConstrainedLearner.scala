package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.{ Annotator, Constants, Sensors }
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, Sentence }
import edu.illinois.cs.cogcomp.lbjava.infer.OJalgoHook
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import edu.illinois.cs.cogcomp.saul.constraint.ConstraintTypeConversion._
import org.allenai.ari.models.Question
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
    x.getView(Constants.VIEW_NAME).getConstituents.asScala.toSeq._forall { c =>
      (learner on c).is(Constants.IMPORTANT_LABEL)
    }
  }

  def getEssentialTermScores(aristoQuestion: Question): Seq[String] = {
    val questionStruct = Annotator.annotateQuestion(aristoQuestion, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(Sensors.stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(
      stopwordConstituents,
      Constants.essentialStopWords
    )
    // update the inverse map with the new constituents
    constituents.foreach(c => Sensors.constituentToAnnotationMap.put(c, questionStruct))
    expandedDataModel.essentialTermTokens.populate(constituents)
    constituents.collect { case c if learner(c) == Constants.IMPORTANT_LABEL => c.getSurfaceForm } ++
      essentialConstituents.map(_.getSurfaceForm)
  }
}

object ConstrainedLearner extends Logging {
  /** Make a new constrained version of [[ExpandedLearner]].
    * Note that for using this constrained classifier you definitely have to have the other models saved on disk.
    */
  def makeNewLearner(
    expandedLearner: ExpandedLearner,
    expandedDataModel: ExpandedDataModel
  ): ConstrainedLearner = {
    new ConstrainedLearner(expandedDataModel, expandedLearner)
  }
}

