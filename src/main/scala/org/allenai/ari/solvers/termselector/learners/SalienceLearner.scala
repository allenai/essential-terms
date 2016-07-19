package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.{ Annotator, Constants, Sensors }
import org.allenai.common.Logging
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn.{ Learner, SparseNetworkLearner }

/** @param baselineDataModel
  * @param useMax whether to use max or summation. If true, would return maxSalience; sumSalience otherwise
  */
class SalienceLearner(baselineDataModel: BaselineDataModel, useMax: Boolean) extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

  override def dataModel: IllinoisDataModel = baselineDataModel

  override def label = dataModel.goldLabel

  override lazy val classifier: Learner = new SparseNetworkLearner()

  override def feature = List.empty

  override def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    val questionStruct = Annotator.annotateQuestion(aristoQuestion, None, None)
    val (stopwordConstituents, constituents) = questionStruct
      .splitConstituents(Sensors.stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct
      .splitConstituents(stopwordConstituents, Constants.essentialStopWords)
    // update the inverse map with the new constituents
    logger.debug("MaxSalience: " + questionStruct.maxSalience)
    logger.debug("SumSalience: " + questionStruct.sumSalience)
    constituents.foreach(c =>
      Sensors.constituentToAnnotationMap.put(c, questionStruct))
    (constituents.map(c => c.getSurfaceForm -> (if (useMax) baselineDataModel.maxSalience(c) else baselineDataModel.sumSalience(c))) // ++
    /*essentialConstituents.map { c => (c.getSurfaceForm, ESSENTIAL_STOPWORD_SCORE) } ++
      nonEssentialConstituents.map { c => (c.getSurfaceForm, NONESSENTIAL_STOPWORD_SCORE) }*/ ).toMap
  }

  override def getEssentialTerms(aristoQuestion: Question, threshold: Double): Seq[String] = {
    val scores = getEssentialTermScores(aristoQuestion)
    scores.collect { case (str, score) if score > threshold => str }.toSeq
  }

  override def predictProbOfBeingEssential(c: Constituent): Double = {
    if (useMax) baselineDataModel.maxSalience(c) else baselineDataModel.sumSalience(c)
  }

  override def predictLabel(c: Constituent, threshold: Double): String = {
    if (predictProbOfBeingEssential(c) > threshold) {
      Constants.IMPORTANT_LABEL
    } else {
      Constants.UNIMPORTANT_LABEL
    }
  }
}

object SalienceLearner extends Logging {
  def makeNewLearners(): SalienceLearners = {
    val baselineDataModel = new BaselineDataModel
    val max = new SalienceLearner(baselineDataModel, true)
    val sum = new SalienceLearner(baselineDataModel, false)
    SalienceLearners(max, sum)
  }
}

case class SalienceLearners(max: SalienceLearner, sum: SalienceLearner)

