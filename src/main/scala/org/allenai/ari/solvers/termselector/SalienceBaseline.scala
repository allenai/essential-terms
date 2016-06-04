package org.allenai.ari.solvers.termselector

import EssentialTermsSensors._
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

import edu.illinois.cs.cogcomp.lbjava.learn.{SparseNetworkLearner, Learner}
import org.allenai.ari.models.Question
import org.allenai.common.Logging

/**
  * @param essentialTermsDataModel
  * @param useMax whether to use max or summation
  */
class SalienceBaseline(essentialTermsDataModel: ExpandedDataModel, useMax: Boolean, threshold: Double
                      ) extends IllinoisLearner(essentialTermsDataModel) with EssentialTermsLearner {

  override def dataModel: IllinoisDataModel = essentialTermsDataModel

  override def label = dataModel.goldLabel

  override lazy val classifier: Learner = new SparseNetworkLearner()

  override def feature = List.empty

  override def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    val questionStruct = annotateQuestion(aristoQuestion, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    println(questionStruct)
    println(questionStruct.maxSalience)
    println(questionStruct.sumSalience)
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    val a = (constituents.map(c => c.getSurfaceForm -> (if (useMax) essentialTermsDataModel.maxSalience(c) else essentialTermsDataModel.sumSalience(c)))// ++
      /*essentialConstituents.map { c => (c.getSurfaceForm, ESSENTIAL_STOPWORD_SCORE) } ++
      nonEssentialConstituents.map { c => (c.getSurfaceForm, NONESSENTIAL_STOPWORD_SCORE) }*/).toMap
    println("bluh . . . ")
    println(a)
    a
  }

  override def getEssentialTerms(aristoQuestion: Question): Seq[String] = {
    val scores = getEssentialTermScores(aristoQuestion)
    scores.collect { case (str, score) if score > threshold => str }.toSeq
  }
}

object SalienceBaseline extends Logging {
  def makeNewLearners(): IllinoisLearner = {
    val (baselineDataModel, baselineLearners) = BaselineLearner.makeNewLearners(loadSavedModel = false)
    val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners)
    val salienceBaseline = new SalienceBaseline(expandedDataModel, true, 0.2)
    salienceBaseline
  }

  /** given a set of training instances it returns the optimal threshold */
  def tuneThreshold(trainingInstances: List[Constituent]): Double = {
    1.0
  }

}

