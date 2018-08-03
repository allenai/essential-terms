package org.allenai.ari.solvers.termselector.evaluation.out

import akka.actor.ActorSystem
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.ari.solvers.termselector.params.{LearnerParams, ServiceParams}
import org.allenai.ari.solvers.termselector.{Annotator, Constants, Sensors, Utils}
import org.allenai.common.Logging

import scala.language.postfixOps

/** An EssentialTermsApp companion object with main() method. */
object RunDirectAnswerAnnotator extends Logging {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("ari-http-solver")

    val loadModelType = LoadFromDatastore
    val classifierModel = "SVM-directAnswer"
    val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
    val localConfig = rootConfig.getConfig("ari.solvers.termselector")
    val modifiedConfig = localConfig.withValue("classifierModel", ConfigValueFactory.fromAnyRef(classifierModel))
    val learnerParams = LearnerParams.fromConfig(modifiedConfig)
    val serviceParams = ServiceParams.fromConfig(modifiedConfig)

    // load the sensors
    val sensors = new Sensors(serviceParams)

    // lazily create the baseline and expanded data models and learners
    // baseline-train is used independently, while baseline-dev is used within expanded learner as feature
    lazy val (baselineDataModelTrain, baselineLearnersTrain) =
      BaselineLearners.makeNewLearners(sensors, learnerParams, "train", loadModelType)
    lazy val (baselineDataModelDev, baselineLearnersDev) =
      BaselineLearners.makeNewLearners(sensors, learnerParams, "dev", loadModelType)
    lazy val salienceLearners = SalienceLearner.makeNewLearners(sensors, directAnswerQuestions = true)
    lazy val (expandedDataModel, expandedLearner) = ExpandedLearner.makeNewLearner(sensors, learnerParams,
      classifierModel, loadModelType, baselineLearnersDev, baselineDataModelDev, salienceLearners)


    val q = "In New York State, the longest period of daylight occurs during which month? "
    val aristoQuestion = Question(q, Some(q), Seq.empty)
    val essentialTerms = expandedLearner.getEssentialTerms(
      aristoQuestion,
      threshold = Constants.EXPANDED_LEARNER_THRESHOLD_DIRECT_ANSWER
    )
    println("Identified essential terms: " + essentialTerms.mkString("/"))
    println(expandedLearner.getEssentialTermScores(aristoQuestion).toString)

    system.terminate()
  }
}
