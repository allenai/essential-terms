package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Sentence, Constituent }
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.learners.{ ExpandedDataModel, IllinoisLearner }

object QuestionHelpers {
  def getEssentialTermProbForAristoQuestion(
    aristoQ: Question,
    learner: IllinoisLearner
  ): Map[String, Double] = {
    val questionStruct = Annotations.annotateQuestion(aristoQ, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(EssentialTermsSensors.stopWords)
    val (essentialConstituents, nonEssentialConstituents) =
      questionStruct.getConstituents(stopwordConstituents, Constants.essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => EssentialTermsSensors.constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.essentialTermTokens.populate(constituents)
    (constituents.map { c => (c.getSurfaceForm, learner.predictProbOfBeingEssential(c)) } ++
      essentialConstituents.map { c => (c.getSurfaceForm, Constants.ESSENTIAL_STOPWORD_SCORE) } ++
      nonEssentialConstituents.map { c => (c.getSurfaceForm, Constants.NONESSENTIAL_STOPWORD_SCORE) }).toMap
  }

  def getEssentialTermsForAristoQuestion(
    aristoQ: Question,
    learner: IllinoisLearner,
    threshold: Double
  ): Seq[String] = {
    val questionStruct = Annotations.annotateQuestion(aristoQ, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(EssentialTermsSensors.stopWords)
    val (essentialConstituents, nonEssentialConstituents) =
      questionStruct.getConstituents(stopwordConstituents, Constants.essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => EssentialTermsSensors.constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.essentialTermTokens.populate(constituents)
    constituents.collect { case c if learner.predictIsEssential(c, threshold) => c.getSurfaceForm } ++
      essentialConstituents.map(_.getSurfaceForm)
  }

  def getEssentialTermsForAristoQuestionConstrainedLearner(
    aristoQ: Question,
    dataModel: ExpandedDataModel,
    learner: ConstrainedClassifier[Constituent, Sentence]
  ): Seq[String] = {
    val questionStruct = Annotations.annotateQuestion(aristoQ, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(EssentialTermsSensors.stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(
      stopwordConstituents,
      Constants.essentialStopWords
    )
    // update the inverse map with the new constituents
    constituents.foreach(c => EssentialTermsSensors.constituentToAnnotationMap.put(c, questionStruct))
    dataModel.essentialTermTokens.populate(constituents)
    constituents.collect { case c if learner(c) == Constants.IMPORTANT_LABEL => c.getSurfaceForm } ++
      essentialConstituents.map(_.getSurfaceForm)
  }
}
