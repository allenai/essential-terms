package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.datamodel.Question
import org.allenai.ari.solvers.termselector.{ Constants, Sensors }
import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn.{ Learner, SparseNetworkLearner }

/** @param baselineDataModel
  * @param useMax whether to use max or summation. If true, would return maxSalience; sumSalience otherwise
  */
class SalienceLearner(
    baselineDataModel: BaselineDataModel,
    useMax: Boolean
) extends IllinoisLearner(baselineDataModel) with EssentialTermsLearner {

  override def dataModel: IllinoisDataModel = baselineDataModel

  override def label = dataModel.goldLabel

  override lazy val classifier: Learner = new SparseNetworkLearner()

  override def feature = List.empty

  override val sensors = baselineDataModel.sensors

  override def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    val questionStruct = sensors.annotator.annotateQuestion(aristoQuestion, None, None)
    val (stopwordConstituents, constituents) = sensors.splitConstituents(questionStruct, sensors.stopWords)
    val (essentialConstituents, nonEssentialConstituents) = sensors.splitConstituents(stopwordConstituents, Constants.ESSENTIAL_STOPWORDS)
    // update the inverse map with the new constituents
    logger.debug("MaxSalience: " + questionStruct.maxSalience)
    logger.debug("SumSalience: " + questionStruct.sumSalience)
    constituents.foreach(c =>
      sensors.constituentToAnnotationMap.put(c, questionStruct))
    constituents.map { c =>
      c.getSurfaceForm -> (if (useMax) baselineDataModel.maxSalience(c) else baselineDataModel.sumSalience(c))
    }.toMap
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
  def makeNewLearners(sensors: Sensors, directAnswerQuestions: Boolean): SalienceLearners = {
    val baselineDataModel = new BaselineDataModel(sensors)
    val (max, sum) = if (!directAnswerQuestions) {
      (new SalienceLearner(baselineDataModel, true), new SalienceLearner(baselineDataModel, false))
    } else {
      val dummySalienceLearner = new DummySalienceLearner(baselineDataModel)
      (dummySalienceLearner, dummySalienceLearner)
    }
    SalienceLearners(max, sum)
  }
}

/** a dummy salience learner, which always returns zero; the parameter `baselinDataModel`
  * isn't actually used by a dummy salience learner (since it always returns 0), but is
  * needed to instantiate the parent class `SalienceLearner`
  */
class DummySalienceLearner(baselineDataModel: BaselineDataModel) extends SalienceLearner(baselineDataModel, true) {

  override def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = Map.empty

  override def getEssentialTerms(aristoQuestion: Question, threshold: Double): Seq[String] = Seq.empty

  override def predictProbOfBeingEssential(c: Constituent): Double = 0.0

  override def predictLabel(c: Constituent, threshold: Double): String = Constants.IMPORTANT_LABEL
}

/** a thin wrapper to contain all possible variables of Salience-based baselines */
case class SalienceLearners(max: SalienceLearner, sum: SalienceLearner)

