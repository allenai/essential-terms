package org.allenai.ari.solvers.termselector

import EssentialTermsSensors._
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn.{Learner, SparseNetworkLearner}
import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector
import org.allenai.common.Logging

/** @param essentialTermsDataModel
  * @param useMax whether to use max or summation
  */
class SalienceBaseline(essentialTermsDataModel: ExpandedDataModel, useMax: Boolean, threshold: Double) extends IllinoisLearner(essentialTermsDataModel) with EssentialTermsLearner {

  override def dataModel: IllinoisDataModel = essentialTermsDataModel

  override def label = dataModel.goldLabel

  override lazy val classifier: Learner = new SparseNetworkLearner()

  override def feature = List.empty

  override def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    val questionStruct = annotateQuestion(aristoQuestion, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    logger.debug("MaxSalience: " + questionStruct.maxSalience)
    logger.debug("SumSalience: " + questionStruct.sumSalience)
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    (constituents.map(c => c.getSurfaceForm -> (if (useMax) essentialTermsDataModel.maxSalience(c) else essentialTermsDataModel.sumSalience(c))) // ++
      /*essentialConstituents.map { c => (c.getSurfaceForm, ESSENTIAL_STOPWORD_SCORE) } ++
      nonEssentialConstituents.map { c => (c.getSurfaceForm, NONESSENTIAL_STOPWORD_SCORE) }*/).toMap
  }

  override def getEssentialTerms(aristoQuestion: Question): Seq[String] = {
    val scores = getEssentialTermScores(aristoQuestion)
    scores.collect { case (str, score) if score > threshold => str }.toSeq
  }

  override def predictProbOfBeingEssential(c: Constituent): Double = {
    if (useMax) essentialTermsDataModel.maxSalience(c) else essentialTermsDataModel.sumSalience(c)
  }

  override def predictLabel(c: Constituent): String = {
    if (predictProbOfBeingEssential(c) > threshold) {
      EssentialTermsConstants.IMPORTANT_LABEL
    } else {
      EssentialTermsConstants.UNIMPORTANT_LABEL
    }
  }
}

object SalienceBaseline extends Logging {
  def makeNewLearners(): IllinoisLearner = {
    val (baselineDataModel, baselineLearners) = BaselineLearner.makeNewLearners(loadSavedModel = false)
    val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners)
    val salienceBaseline = new SalienceBaseline(expandedDataModel, true, 0)
    salienceBaseline
  }
}

