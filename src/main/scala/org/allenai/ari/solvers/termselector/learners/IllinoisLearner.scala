package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.{ Constants, EssentialTermsSensors, QuestionHelpers }
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn.StochasticGradientDescent
import edu.illinois.cs.cogcomp.saul.classifier.Learnable

/** how to load the models */
sealed trait LoadType
case object LoadFromDisk extends LoadType { override def toString = "Load the pre-trained model from disk" }
case object LoadFromDatastore extends LoadType { override def toString = "Load the pre-trained model from datastore" }
case object TrainModel extends LoadType { override def toString = "Train a new model" }

/** A parameterized abstract class for UIUC learners for essential terms detection. */
abstract class IllinoisLearner(
    essentialTermsDataModel: IllinoisDataModel
) extends Learnable[Constituent](essentialTermsDataModel.essentialTermTokens) with EssentialTermsLearner {

  /** This allows access to sub-classes of EssentialTermsDataModel if set appropriately by
    * inheriting classes.
    */
  def dataModel: IllinoisDataModel

  // implement for trait MyLearner
  def getEssentialTermScores(aristoQuestion: Question): Map[String, Double] = {
    QuestionHelpers.getEssentialTermProbForAristoQuestion(aristoQuestion, this)
  }

  // implement for trait MyLearner
  def getEssentialTerms(aristoQuestion: Question, threshold: Double): Seq[String] = {
    QuestionHelpers.getEssentialTermsForAristoQuestion(aristoQuestion, this, threshold)
  }

  /** Predict the class label of a given term. */
  def predictLabel(c: Constituent, threshold: Double): String = {
    if (predictProbOfBeingEssential(c) > threshold) {
      Constants.IMPORTANT_LABEL
    } else {
      Constants.UNIMPORTANT_LABEL
    }
  }

  /** Predict whether a given term is essential. */
  def predictIsEssential(c: Constituent, threshold: Double): Boolean = {
    predictLabel(c, threshold) == Constants.IMPORTANT_LABEL
  }

  /** Predict the probability of a given term being essential. */
  def predictProbOfBeingEssential(c: Constituent): Double = {
    val rawScore = if (classifier.isInstanceOf[StochasticGradientDescent]) {
      classifier.realValue(c)
    } else {
      val scores = classifier.scores(c).toArray
      scores.find {
        case score => score.value == Constants.IMPORTANT_LABEL
      }.get.score
    }
    convertRealToProbability(rawScore)
  }

  private def convertRealToProbability(realScore: Double): Double = {
    1 / (1 + Math.exp(-realScore))
  }
}
