package org.allenai.ari.solvers.termselector

import org.allenai.ari.datamodel.Question
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.ari.solvers.termselector.params.{ LearnerParams, ServiceParams }
import org.allenai.common.Logging

import akka.actor.ActorSystem
import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.config.{ Config, ConfigFactory, ConfigValueFactory }
import spray.json._
import spray.json.DefaultJsonProtocol._

/** A service for identifying essential terms in Aristo questions. The recommended way to use the system (especially
  * if you're using Guice Injection) is to use EssentialTermServiceFactory (see below).
  * @param learnerParams the parameters necessary that characterize the learnr type used in prediction of essential terms
  * @param sensors an object containing the necessary annotations, caches and related methods, necessary for
  * feature extraction used in the learners
  */
class EssentialTermsService(
    val learnerParams: LearnerParams,
    val sensors: Sensors
)(implicit actorSystem: ActorSystem) extends Logging {

  /** Create a learner object, with default thresholds chosen to maximize F1 on the dev set. */
  val (learner, defaultThreshold) = {
    logger.info(s"Initializing essential terms service with learner type: ${learnerParams.classifierType}")
    learnerParams.classifierType match {
      case "Lookup" => (new LookupLearner(sensors), Constants.LOOKUP_THRESHOLD)
      case "MaxSalience" => (SalienceLearner.makeNewLearners(sensors, learnerParams.directAnswerQuestions).max, Constants.MAX_SALIENCE_THRESHOLD)
      case "SumSalience" => (SalienceLearner.makeNewLearners(sensors, learnerParams.directAnswerQuestions).sum, Constants.SUM_SALIENCE_THRESHOLD)
      case "LemmaBaseline" => (BaselineLearners.makeNewLearners(sensors, learnerParams, "train", loadModelType = LoadFromDatastore)._2.lemma, Constants.LEMMA_BASELINE_THRESHOLD)
      case "Expanded" =>
        val salienceBaselines = SalienceLearner.makeNewLearners(sensors, learnerParams.directAnswerQuestions)
        val (baselineDataModel, baselineClassifiers) = BaselineLearners.makeNewLearners(sensors, learnerParams, "dev", LoadFromDatastore)
        (ExpandedLearner.makeNewLearner(sensors, learnerParams, learnerParams.classifierModel, LoadFromDatastore,
          baselineClassifiers, baselineDataModel, salienceBaselines)._2, Constants.EXPANDED_LEARNER_THRESHOLD)
      case _ => throw new IllegalArgumentException(s"Unidentified learner type ${learnerParams.classifierType}")
    }
  }
  def uniqueCacheName = learnerParams.classifierType + learnerParams.classifierModel

  /** Get essential term scores for a given question.
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @return a map of the terms and their importance
    */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    if (learnerParams.useRedisCachingForLearnerPredictions) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
  }

  /** Get essential terms for a given question; use threshold if provided, otherwise defaultThreshold
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @param threshold the threshold above which a term is considered essential
    * @return a sequence of essential terms in the input questions
    */
  def getEssentialTerms(aristoQ: Question, threshold: Double = defaultThreshold): Seq[String] = {
    require(threshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = if (learnerParams.useRedisCachingForLearnerPredictions) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
    termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
  }

  /** Get essential terms for a given question (selected via threshold, if provided; otherwise defaultThreshold),
    * as well as essential term scores for a given question.
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @param threshold the threshold above which a term is considered essential
    * @return a tuple containing a sequence of essential terms as well as a hashmap of terms and their essentiality scores
    */
  def getEssentialTermsAndScores(aristoQ: Question, threshold: Double = defaultThreshold): (Seq[String], Map[String, Double]) = {
    require(threshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = if (learnerParams.useRedisCachingForLearnerPredictions) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
    val essentialTerms = termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
    (essentialTerms, termsWithScores)
  }

  /** Retrieve essential scores from Redis cache; if not present, compute and store.
    * @param aristoQ an input question in Aristo's standard question datastructure
    * @return a hashmap of the terms and their importance
    */
  private def getEssentialScoresFromRedis(aristoQ: Question): Map[String, Double] = {
    // use the raw question in the cache as the essential term prediction depends on the options
    val cacheKey = "EssentialTermsServiceCache***scores***" + aristoQ.rawQuestion + uniqueCacheName
    val termsAndScoreJsonOpt = sensors.annotator.synchronizedRedisClient.get(cacheKey)
    termsAndScoreJsonOpt match {
      case Some(termsAndScoreJson) =>
        termsAndScoreJson.parseJson.convertTo[Map[String, Double]]
      case None =>
        val scores = learner.getEssentialTermScores(aristoQ)
        sensors.annotator.synchronizedRedisClient.put(
          cacheKey, scores.toJson.compactPrint
        )
        scores
    }
  }
}

object EssentialTermsService {
  val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
  val localConfig = rootConfig.getConfig("ari.solvers.termselector")

  // empty constructor
  def apply()(implicit actorSystem: ActorSystem): EssentialTermsService = {
    new EssentialTermServiceFactory(ConfigFactory.empty()).getInstance()
  }

  /** @param classifierType whether and how to identify and use essential terms in the model. Example values are
    * "LemmaBaseline", "Expanded", "Lookup", "MaxSalience", etc.
    * @param classifierModel the type of the underlying model used for predictions (e.g. SVM, etc).
    * This parameter is set, only when the first parameter is set to "Expanded"
    * @return a learner with its parameters included
    */
  def apply(classifierType: String, classifierModel: String = "")(implicit actorSystem: ActorSystem): EssentialTermsService = {
    val modifiedConfig = ConfigFactory.empty().
      withValue("classifierModel", ConfigValueFactory.fromAnyRef(classifierModel)).
      withValue("classifierType", ConfigValueFactory.fromAnyRef(classifierType))
    new EssentialTermServiceFactory(modifiedConfig).getInstance()
  }

  /** This constructor can be used to save some time/memory when testing multiple classifiers at the same time.
    * Sensors, which uses significant memory/time-expensive can be shared among the classifiers.
    */
  def apply(classifierType: String, classifierModel: String, sensors: Sensors)(implicit actorSystem: ActorSystem): EssentialTermsService = {
    val modifiedConfig = localConfig.
      withValue("classifierModel", ConfigValueFactory.fromAnyRef(classifierModel)).
      withValue("classifierType", ConfigValueFactory.fromAnyRef(classifierType))
    val learnerParams = LearnerParams.fromConfig(modifiedConfig)
    new EssentialTermsService(learnerParams, sensors)
  }
}

/** The main class used when using essential-terms with injection. It injects the an instances of EssentialTermServiceFactory.
  * Then `getInstance` can be called to instantiate an istance of EssentialTermService, which can be used to extract
  * the essential terms. If you want to override any of the settings for essential-terms, simply put then insidie a block
  * with prefix "termselector.local".
  * @param config the config file automatically injected
  */
class EssentialTermServiceFactory @Inject() (@Named("termselector.local") config: Config) {
  def getInstance()(implicit actorSystem: ActorSystem): EssentialTermsService = {
    val configWithFallbackOnLocalConfig = config.withFallback(EssentialTermsService.localConfig)
    val learnerParams = LearnerParams.fromConfig(configWithFallbackOnLocalConfig)
    val serviceParams = ServiceParams.fromConfig(configWithFallbackOnLocalConfig)
    val sensors = new Sensors(serviceParams)
    new EssentialTermsService(learnerParams, sensors)
  }
}