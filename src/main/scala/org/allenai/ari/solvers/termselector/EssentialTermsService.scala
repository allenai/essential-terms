package org.allenai.ari.solvers.termselector

import com.typesafe.config.{ Config, ConfigFactory }
import org.allenai.ari.models.Question
import org.allenai.ari.solvers.termselector.learners._
import org.allenai.common.Logging

import com.google.inject.{ ImplementedBy, Inject }
import com.google.inject.name.Named
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.collection.JavaConverters._

/** a thin trait to contain a learner with its threshold */
@ImplementedBy(classOf[InjectedLearnerAndThreshold])
trait LearnerAndThreshold {
  def learner: EssentialTermsLearner
  def threshold: Double
  def uniqueCacheName: String
  def directAnswerQuestions: Boolean
  def useRedisCaching: Boolean
  def stopwordsDatastoreFile: String
  def filterMidScoreConsitutents: Seq[Double]
  def scienceTermsDatastoreFile: String
  def regentsTrainingQuestion: String
  def checkForMissingSalienceScores: Boolean
  def turkerEssentialityScores: String
  def combineNamedEntities: Boolean
}

/** pair of learner and threshold with injected parameters
  *
  * @param classifierType whether and how to identify and use essential terms in the model
  * @param classifierModel the type of the underlying model used for predictions
  * @param useRedisCaching whether to cache the output scores in a redis cache; would require you to run redis upon using
  */
class InjectedLearnerAndThreshold @Inject() (
    @Named("termselector.classifierType") classifierType: String,
    @Named("termselector.classifierModel") classifierModel: String,
    @Named("termselector.directAnswerQuestions") val directAnswerQuestions: Boolean,
    @Named("termselector.useRedisCaching") val useRedisCaching: Boolean,
    @Named("termselector.stopwordsDatastoreFile") val stopwordsDatastoreFile: String,
    @Named("termselector.filterMidScoreConsitutents") val filterMidScoreConsitutents: List[Double],
    @Named("termselector.scienceTermsDatastoreFile") val scienceTermsDatastoreFile: String,
    @Named("termselector.regentsTrainingQuestion") val regentsTrainingQuestion: String,
    @Named("termselector.checkForMissingSalienceScores") val checkForMissingSalienceScores: Boolean,
    @Named("termselector.turkerEssentialityScores") val turkerEssentialityScores: String,
    @Named("termselector.combineNamedEntities") val combineNamedEntities: Boolean
) extends LearnerAndThreshold with Logging {
  override val (learner, threshold) = {
    logger.info(s"Initializing essential terms service with learner type: $classifierType")
    classifierType match {
      case "Lookup" => (new LookupLearner(), Constants.LOOKUP_THRESHOLD)
      case "MaxSalience" => (SalienceLearner.makeNewLearners().max, Constants.MAX_SALIENCE_THRESHOLD)
      case "SumSalience" => (SalienceLearner.makeNewLearners().sum, Constants.SUM_SALIENCE_THRESHOLD)
      case "LemmaBaseline" => (BaselineLearners.makeNewLearners(
        loadModelType = LoadFromDatastore, "train"
      )._2.lemma, Constants.LEMMA_BASELINE_THRESHOLD)
      case "Expanded" =>
        val salienceBaselines = SalienceLearner.makeNewLearners()
        val (baselineDataModel, baselineClassifiers) =
          BaselineLearners.makeNewLearners(LoadFromDatastore, "dev")
        (ExpandedLearner.makeNewLearner(LoadFromDatastore, classifierModel, baselineClassifiers,
          baselineDataModel, salienceBaselines)._2, Constants.EXPANDED_LEARNER_THRESHOLD)
      case _ => throw new IllegalArgumentException(s"Unidentified learner type $classifierType")
    }
  }
  override def uniqueCacheName = classifierType + classifierModel
}

object InjectedLearnerAndThreshold {
  // reading the default config file
  private val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
  private val localConfig = rootConfig.getConfig("ari.solvers.termselector")

  // empty constructor
  def apply(): InjectedLearnerAndThreshold = apply(localConfig)

  // constructor with arbitrary config files
  def apply(newConfig: Config): InjectedLearnerAndThreshold = {
    val configWithFallbackOnLocalConfig = newConfig.withFallback(localConfig)
    new InjectedLearnerAndThreshold(
      configWithFallbackOnLocalConfig.getString("termselector.classifierType"),
      configWithFallbackOnLocalConfig.getString("termselector.classifierModel"),
      configWithFallbackOnLocalConfig.getBoolean("termselector.directAnswerQuestions"),
      configWithFallbackOnLocalConfig.getBoolean("termselector.useRedisCaching"),
      configWithFallbackOnLocalConfig.getString("termselector.stopwordsDatastoreFile"),
      configWithFallbackOnLocalConfig.getDoubleList("termselector.filterMidScoreConsitutents").asScala.map(_.doubleValue()).toList,
      configWithFallbackOnLocalConfig.getString("termselector.scienceTermsDatastoreFile"),
      configWithFallbackOnLocalConfig.getString("termselector.regentsTrainingQuestion"),
      configWithFallbackOnLocalConfig.getBoolean("termselector.checkForMissingSalienceScores"),
      configWithFallbackOnLocalConfig.getString("termselector.turkerEssentialityScores"),
      configWithFallbackOnLocalConfig.getBoolean("termselector.combineNamedEntities")
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
  directAnswerQuestions: Boolean,
  useRedisCaching: Boolean,
  stopwordsDatastoreFile: String,
  filterMidScoreConsitutents: Seq[Double],
  scienceTermsDatastoreFile: String,
  regentsTrainingQuestion: String,
  checkForMissingSalienceScores: Boolean,
  turkerEssentialityScores: String,
  combineNamedEntities: Boolean
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

  /** create sensors and annotators */
  val sensors = new Sensors(
    learnerAndThreshold.stopwordsDatastoreFile,
    learnerAndThreshold.filterMidScoreConsitutents,
    learnerAndThreshold.scienceTermsDatastoreFile,
    learnerAndThreshold.regentsTrainingQuestion,
    learnerAndThreshold.checkForMissingSalienceScores,
    learnerAndThreshold.useRedisCaching,
    learnerAndThreshold.turkerEssentialityScores,
    learnerAndThreshold.combineNamedEntities
  )

  /** Get essential term scores for a given question.
    *
    * @param aristoQ an input question, in Aristo's standard datastructure for questions
    * @return a map of the terms and their importance
    */
  def getEssentialTermScores(aristoQ: Question): Map[String, Double] = {
    if (learnerAndThreshold.useRedisCaching) {
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
    val termsWithScores = if (learnerAndThreshold.useRedisCaching) {
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
    val termsWithScores = if (learnerAndThreshold.useRedisCaching) {
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
    val termsAndScoreJsonOpt = sensors.synchronized {
      sensors.annnotator.synchronizedRedisClient.get(cacheKey)
    }
    termsAndScoreJsonOpt match {
      case Some(termsAndScoreJson) =>
        termsAndScoreJson.parseJson.convertTo[Map[String, Double]]
      case None =>
        val scores = learner.getEssentialTermScores(aristoQ)
        sensors.annnotator.synchronizedRedisClient.put(
          cacheKey, scores.toJson.compactPrint
        )
        scores
    }
  }
}
