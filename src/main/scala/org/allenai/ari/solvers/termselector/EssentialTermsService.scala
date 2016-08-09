package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.ari.solvers.termselector.params.{ LearnerParams, ServiceParams }
import org.allenai.common.Logging

import com.google.inject.{ ImplementedBy, Inject }
import com.typesafe.config.{ Config, ConfigFactory, ConfigValueFactory }
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.collection.JavaConverters._

/** a thin trait to contain a learner with its threshold */
@ImplementedBy(classOf[InjectedLearnerAndThreshold])
trait LearnerAndThreshold {
  def learner: EssentialTermsLearner
  def threshold: Double
  def uniqueCacheName: String
  def learnerParams: LearnerParams
  def serviceParams: ServiceParams
  def sensors: Sensors
}

/** a class to construct a learner given its parameters
  *
  * @param learnerParams Inject-able parameters necessary to initialize a learner
  */
class InjectedLearnerAndThreshold @Inject() (
    val learnerParams: LearnerParams,
    val serviceParams: ServiceParams
) extends LearnerAndThreshold with Logging {

  /** create sensors and annotators */
  override val sensors = new Sensors(serviceParams)

  override val (learner, threshold) = {
    logger.info(s"Initializing essential terms service with learner type: ${learnerParams.classifierType}")
    learnerParams.classifierType match {
      case "Lookup" => (new LookupLearner(sensors), Constants.LOOKUP_THRESHOLD)
      case "MaxSalience" => (SalienceLearner.makeNewLearners(sensors, learnerParams.directAnswerQuestions).max, Constants.MAX_SALIENCE_THRESHOLD)
      case "SumSalience" => (SalienceLearner.makeNewLearners(sensors, learnerParams.directAnswerQuestions).sum, Constants.SUM_SALIENCE_THRESHOLD)
      case "LemmaBaseline" => (BaselineLearners.makeNewLearners(sensors, learnerParams, "train", loadModelType = LoadFromDatastore)._2.lemma, Constants.LEMMA_BASELINE_THRESHOLD)
      case "Expanded" =>
        val salienceBaselines = SalienceLearner.makeNewLearners(sensors, learnerParams.directAnswerQuestions)
        val (baselineDataModel, baselineClassifiers) = BaselineLearners.makeNewLearners(sensors, learnerParams, "dev", LoadFromDatastore)
        (ExpandedLearner.makeNewLearner(sensors, learnerParams, LoadFromDatastore,
          baselineClassifiers, baselineDataModel, salienceBaselines)._2, Constants.EXPANDED_LEARNER_THRESHOLD)
      case _ => throw new IllegalArgumentException(s"Unidentified learner type ${learnerParams.classifierType}")
    }
  }
  override def uniqueCacheName = learnerParams.classifierType + learnerParams.classifierModel
}

object InjectedLearnerAndThreshold {
  // reading the default config file
  private val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
  private val localConfig = rootConfig.getConfig("ari.solvers.termselector")

  // empty constructor
  def apply(): InjectedLearnerAndThreshold = apply(localConfig)

  /** @param classifierType whether and how to identify and use essential terms in the model. Example values are
    * "LemmaBaseline", "Expanded", "Lookup", "MaxSalience", etc.
    * @param classifierModel the type of the underlying model used for predictions (e.g. SVM, etc).
    * This parameter is set, only when the first parameter is set to "Expanded"
    * @return a learner with its parameters included
    */
  def apply(classifierType: String, classifierModel: String = ""): InjectedLearnerAndThreshold = {
    val modifiedConfig = localConfig.
      withValue("classifierModel", ConfigValueFactory.fromAnyRef(classifierModel)).
      withValue("classifierType", ConfigValueFactory.fromAnyRef(classifierType))
    apply(modifiedConfig)
  }

  // constructor with arbitrary config files
  def apply(newConfig: Config): InjectedLearnerAndThreshold = {
    val configWithFallbackOnLocalConfig = newConfig.withFallback(localConfig)
    new InjectedLearnerAndThreshold(
      new LearnerParams(
        configWithFallbackOnLocalConfig.getString("classifierType"),
        configWithFallbackOnLocalConfig.getString("classifierModel"),
        configWithFallbackOnLocalConfig.getBoolean("directAnswerQuestions"),
        configWithFallbackOnLocalConfig.getString("modelsDatastoreFolder")
      ),
      new ServiceParams(
        configWithFallbackOnLocalConfig.getString("stopwordsDatastoreFile"),
        configWithFallbackOnLocalConfig.getDoubleList("annotation.filterMidScoreConsitutents").asScala.map(_.doubleValue()).toList,
        configWithFallbackOnLocalConfig.getString("scienceTermsDatastoreFile"),
        configWithFallbackOnLocalConfig.getString("regentsTrainingQuestion"),
        configWithFallbackOnLocalConfig.getBoolean("annotation.checkForMissingSalienceScores"),
        configWithFallbackOnLocalConfig.getString("turkerEssentialityScores"),
        configWithFallbackOnLocalConfig.getBoolean("annotation.combineNamedEntities"),
        configWithFallbackOnLocalConfig.getBoolean("useRedisCaching")
      )
    )
  }
}

/** a simple way to get learner and threshold from input
  *
  * @param learner essential term learner
  * @param threshold threshold used to make binry predictions using confidence scores
  * @param uniqueCacheName the prefix name used while putting values in redis cache
  */
case class SimpleLearnerAndThreshold(
  learner: EssentialTermsLearner,
  threshold: Double,
  uniqueCacheName: String,
  learnerParams: LearnerParams,
  serviceParams: ServiceParams,
  sensors: Sensors
) extends LearnerAndThreshold

/** A service for identifying essential terms in Aristo questions.
  *
  * @param learnerAndThreshold a learner-threshold pair
  */
class EssentialTermsService @Inject() (
    learnerAndThreshold: LearnerAndThreshold
) extends Logging {

  /** Create a learner object. Lazy to avoid creating a learner if the service is not used.
    * The default thresholds are chosen to maximize F1 on the dev set, given the threshold
    */
  private val (learner, defaultThreshold) = (learnerAndThreshold.learner, learnerAndThreshold.threshold)

  /** Get essential term scores for a given question.
    *
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @return a map of the terms and their importance
    */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    if (learnerAndThreshold.serviceParams.useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
  }

  /** Get essential terms for a given question; use threshold if provided, otherwise defaultThreshold
    *
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @param threshold the threshold above which a term is considered essential
    * @return a sequence of essential terms in the input questions
    */
  def getEssentialTerms(aristoQ: Question, threshold: Double = defaultThreshold): Seq[String] = {
    require(threshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = if (learnerAndThreshold.serviceParams.useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
    termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
  }

  /** Get essential terms for a given question (selected via threshold, if provided; otherwise defaultThreshold),
    * as well as essential term scores for a given question.
    *
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @param threshold the threshold above which a term is considered essential
    * @return a tuple containing a sequence of essential terms as well as a hashmap of terms and their essentiality scores
    */
  def getEssentialTermsAndScores(aristoQ: Question, threshold: Double = defaultThreshold): (Seq[String], Map[String, Double]) = {
    require(threshold >= 0, "The defined threshold must be bigger than zero . . . ")
    val termsWithScores = if (learnerAndThreshold.serviceParams.useRedisCaching) {
      getEssentialScoresFromRedis(aristoQ)
    } else {
      learner.getEssentialTermScores(aristoQ)
    }
    val essentialTerms = termsWithScores.collect { case (term, score) if score >= threshold => term }.toSeq
    (essentialTerms, termsWithScores)
  }

  /** Retrieve essential scores from Redis cache; if not present, compute and store.
    *
    * @param aristoQ an input question in Aristo's standard question datastructure
    * @return a hashmap of the terms and their importance
    */
  private def getEssentialScoresFromRedis(
    aristoQ: Question
  ): Map[String, Double] = {
    // use the raw question in the cache as the essential term prediction depends on the options
    val cacheKey = "EssentialTermsServiceCache***scores***" + aristoQ.rawQuestion + learnerAndThreshold.uniqueCacheName
    val termsAndScoreJsonOpt = learnerAndThreshold.sensors.synchronized {
      learnerAndThreshold.sensors.annnotator.synchronizedRedisClient.get(cacheKey)
    }
    termsAndScoreJsonOpt match {
      case Some(termsAndScoreJson) =>
        termsAndScoreJson.parseJson.convertTo[Map[String, Double]]
      case None =>
        val scores = learner.getEssentialTermScores(aristoQ)
        learnerAndThreshold.sensors.annnotator.synchronizedRedisClient.put(
          cacheKey, scores.toJson.compactPrint
        )
        scores
    }
  }
}
