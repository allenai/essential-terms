package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, ParentheticalChoiceIdentifier, Question }
import org.allenai.ari.solvers.common.SolversCommonModule
import org.allenai.ari.solvers.common.salience.SalienceScorer
import org.allenai.ari.solvers.termselector.learners.{ ExpandedDataModel, IllinoisLearner }
import org.allenai.common.{ FileUtils, Logging }
import org.allenai.common.guice.ActorSystemModule
import org.allenai.datastore.Datastore

import ch.qos.logback.classic.Level
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, Sentence, TextAnnotation, TokenLabelView }
import edu.illinois.cs.cogcomp.core.utilities.configuration.{ Configurator, ResourceManager }
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.curator.CuratorConfigurator
import edu.illinois.cs.cogcomp.edison.features.factory.WordFeatureExtractorFactory
import edu.illinois.cs.cogcomp.nlp.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.nlp.pipeline.IllinoisPipelineFactory
import edu.illinois.cs.cogcomp.saul.classifier.ConstrainedClassifier
import akka.actor.ActorSystem
import com.google.inject.Guice
import com.medallia.word2vec.Word2VecModel
import com.redis._
import com.typesafe.config.{ ConfigFactory, ConfigValueFactory }
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector
import spray.json._
import DefaultJsonProtocol._
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Codec
import scala.util.Random
import java.io.File
import java.util.Properties

object EssentialTermsSensors extends Logging {
  lazy val allQuestions = readAndAnnotateEssentialTermsData()

  lazy val preTrainedModels = Datastore("public").directoryPath("org.allenai.termselector", "models", 4)

  lazy val stopWords = {
    lazy val stopWordsFile = Utils.getDatastoreFileAsSource(
      "public", "org.allenai.termselector", "stopwords.txt", 1
    )
    val stopWords = stopWordsFile.getLines().toList
    stopWordsFile.close()
    stopWords.toSet ++ Set("__________")
  }
  // since we never to learning on the above stopwords, we choose a subset of the stopwords to
  // ALWAYS be essential
  lazy val essentialStopWords = Set("all", "any", "because", "before", "both", "but")
  val ESSENTIAL_STOPWORD_SCORE = 0.8
  lazy val nonessentialStopWords = stopWords.--(essentialStopWords)
  val NONESSENTIAL_STOPWORD_SCORE = 0.0

  // a hashmap from sentences to [[TextAnnotation]]s
  // TODO(daniel): this is not used currently; decide if you want to use this file, or use redis
  lazy val annotationFileCache = {
    val annotationCacheFile = Utils.getDatastoreFileAsSource(
      "private", "org.allenai.termselector", "annotationCache.json", 1
    )
    val lines = annotationCacheFile.getLines().toList
    val sentenceAnnnotationMap = lines.map { singleLine =>
      val splitted = singleLine.split("\t")
      splitted(0) -> SerializationHelper.deserializeFromJson(splitted(1))
    }.toMap
    annotationCacheFile.close()
    sentenceAnnnotationMap
  }

  // redis cache for annotations
  // TODO(daniel): create a key in application.conf for activating this feature
  lazy val annotationRedisCache = new RedisClient("localhost", 6379)

  private def redisGet(key: String): Option[String] = {
    try {
      this.synchronized(annotationRedisCache.get(key))
    } catch {
      case e: Exception => {
        logger.error("Fetching information from redis failed!")
        logger.error(s"Key: $key")
        e.printStackTrace()
        None
      }
    }
  }

  private def redisSet(key: String, value: String): Unit = {
    try {
      this.synchronized(annotationRedisCache.set(key, value))
    } catch {
      case e: Exception => {
        logger.error("Setting information in redis failed!")
        logger.error(s"Key: $key")
        e.printStackTrace()
        None
      }
    }
  }

  // TODO(danm): This path should come from config so anyone can run the code.
  private lazy val word2vecFile = new File(
    "/Users/daniel/ideaProjects/convertvec/GoogleNews-vectors-negative300-length=200000.bin"
  )
  lazy val w2vModel = Word2VecModel.fromBinFile(word2vecFile)
  lazy val w2vNoMatchStr = "</s>" // string used by word2vec when there is no match

  lazy val salienceMap = {
    val salienceCache = Utils.getDatastoreFileAsSource(
      "public", "org.allenai.termselector", Constants.SALIENCE_CACHE, 3
    )
    val lines = salienceCache.getLines
    val cache = lines.grouped(2).map {
      case q :: json :: _ =>
        q -> json.parseJson.convertTo[List[(MultipleChoiceSelection, SalienceResult)]]
    }.toMap
    salienceCache.close()
    cache
  }

  // regents training question: just to make sure they are all in the test set of the term-selector
  lazy val regentsSet = {
    val separator = "\",".r
    lazy val rawTextFile = Datastore("private").filePath("org.allenai.tableilp.data", "regentsTrain.txt", 1).toFile
    lazy val questions = FileUtils.getFileAsLines(rawTextFile)
    questions.map { q => decomposeQuestion(separator.replaceAllIn(q, " ").replaceAll("\"", "")).text }
  }

  lazy val (trainConstituents, testConstituents, devConstituents, trainSentences, testSentences, devSentences) = {
    val trainProb = 0.7
    val devProb = 0.4 // 40% after removing training question
    // in order to be consistent across runs
    Random.setSeed(10)
    val (regents, nonRegents) = allQuestions.partition(q => regentsSet.contains(q.aristoQuestion.text))
    val trainSize = (trainProb * allQuestions.size).toInt
    val (train, nonTrainNonRegents) = Random.shuffle(nonRegents).splitAt(trainSize)
    val devSize = (devProb * nonTrainNonRegents.size).toInt
    val (dev, nonDev_nonTrain_nonRegents) = Random.shuffle(nonTrainNonRegents).splitAt(devSize)
    val test = nonDev_nonTrain_nonRegents ++ regents // add regents to the test data
    val trainSen = getSentence(train)
    val testSen = getSentence(test)
    val devSen = getSentence(dev)

    // TODO(daniel): make it parameter in application.conf
    val filterMidScoreConsitutents = false
    val filteredTrainSen = if (filterMidScoreConsitutents) {
      trainSen.map { consList => consList.toList.filter { c => c.getConstituentScore >= 0.65 || c.getConstituentScore <= 0.35 } }
    } else {
      trainSen
    }

    // add a train attribute to the training constituents, in order to make sure they will have
    // different hashcode than the test constituents
    trainSen.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("trainidx", s"$idx") }
    testSen.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("testidx", s"${9999 + idx}") }
    devSen.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("devidx", s"${999999 + idx}") }
    (filteredTrainSen.flatten, testSen.flatten, devSen.flatten, filteredTrainSen, testSen, devSen)
  }

  lazy val allConstituents = trainConstituents ++ testConstituents ++ devConstituents

  lazy val allSentences = trainSentences ++ testSentences ++ devSentences

  // splitting questions based on their types, like wh-question, etc
  lazy val (whatQuestions, whichQuestions, whereQuestions, whenQuestions, howQuestions, nonWhQuestions) = {
    def split(input: Iterable[Constituent], keyword: String) = {
      input.partition { c =>
        val annotation = constituentToAnnotationMap(c)
        annotation.rawQuestion.toLowerCase.contains(keyword)
      }
    }
    val (what, rest1) = split(testConstituents, "what")
    val (which, rest2) = split(rest1, "which")
    val (where, rest3) = split(rest2, "where")
    val (when, rest4) = split(rest3, "when")
    val (how, nonWh) = split(rest4, "how")

    (what, which, where, when, how, nonWh)
  }

  // This creates a map from constituents, to its corresponding [[QuestionStruct]] which contains
  // the annotations of the question containing it.
  // TODO: make this immutable
  lazy val constituentToAnnotationMap = collection.mutable.Map(allQuestions.flatMap { q =>
    val constituents =
      q.questionTextAnnotation
        .getView(Constants.VIEW_NAME)
        .getConstituents
        .asScala
    constituents.map(_ -> q)
  }: _*)

  lazy val (salienceScorer, actorSystem) = {
    loggerConfig.Logger("org.allenai.wumpus.client.WumpusClient").setLevel(Level.ERROR)
    implicit val system = ActorSystem("ari-http-solver")
    val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
    val localConfig = rootConfig.getConfig("ari.solvers.common").withValue(
      "wumpus-overrides",
      ConfigValueFactory.fromMap(Map("redisTimeoutMillis" -> Int.MaxValue.toString).asJava)
    )
    val injector = Guice.createInjector(
      new ActorSystemModule,
      new SolversCommonModule(localConfig, true)
    )
    (injector.instance[SalienceScorer], system)
  }

  /** Load science terms from Datastore */
  lazy val scienceTerms: Set[String] = {
    val datastoreName = "public"
    val group = "org.allenai.nlp.resources"
    val name = "science_terms.txt"
    val version = 1
    val file = Utils.getDatastoreFileAsSource(datastoreName, group, name, version)
    val terms = file.getLines().filterNot(_.startsWith("#")).toSet
    file.close()
    terms
  }

  val brownClusterFeatureExtractor = WordFeatureExtractorFactory.getBrownFeatureGenerator(
    "",
    "brown-clusters/brown-rcv1.clean.tokenized-CoNLL03.txt-c100-freq1.txt", Array[Int](4, 5)
  )

  def getConstituentAfter(x: Constituent, viewName: String = ViewNames.TOKENS): Constituent = {
    val consAfter = x.getTextAnnotation.getView(viewName).getConstituents.asScala.
      filter(cons => cons.getStartSpan >= x.getEndSpan)
    if (consAfter.nonEmpty) consAfter.minBy(_.getEndSpan) else x
  }

  def getConstituentBefore(x: Constituent, viewName: String = ViewNames.TOKENS): Constituent = {
    val consBefore = x.getTextAnnotation.getView(viewName).getConstituents.asScala.
      filter(cons => cons.getEndSpan <= x.getStartSpan)
    if (consBefore.nonEmpty) consBefore.maxBy(_.getEndSpan) else x
  }

  def getConstituentTwoAfter(x: Constituent, viewName: String = ViewNames.TOKENS): Constituent = {
    val consAfter = x.getTextAnnotation.getView(viewName).getConstituents.asScala.
      filter(cons => cons.getStartSpan >= x.getEndSpan)
    if (consAfter.size >= 2) consAfter.sortBy(_.getEndSpan).toList(1) else x
  }

  def getConstituentTwoBefore(x: Constituent, viewName: String = ViewNames.TOKENS): Constituent = {
    val consBefore = x.getTextAnnotation.getView(viewName).getConstituents.asScala.
      filter(cons => cons.getEndSpan <= x.getStartSpan)
    if (consBefore.size >= 2) consBefore.sortBy(-_.getEndSpan).toList(1) else x
  }

  def getConstituentCoveringInView(c: Constituent, view: String): java.util.List[Constituent] = {
    c.getTextAnnotation.getView(view).getConstituentsCovering(c)
  }

  private def readAndAnnotateEssentialTermsData(): Seq[EssentialTermsQuestion] = {
    // only master train: turkerSalientTerms.tsv
    // only omnibus: turkerSalientTermsOnlyOmnibus.tsv
    // combined: turkerSalientTermsWithOmnibus.tsv
    val salientTermsFile = Datastore("private").filePath(
      "org.allenai.termselector", "turkerSalientTermsWithOmnibus.tsv", 3
    ).toFile
    // Some terms in the turker generated file need ISO-8859 encoding
    val allQuestions = FileUtils.getFileAsLines(salientTermsFile)(Codec.ISO8859).map { line =>
      val fields = line.split("\t")
      require(fields.size == 3, s"Expected format: question numAnnotators word-counts. Got: $line")
      val question = fields(0)
      val numAnnotators = fields(1).toDouble
      val wordCounts = fields(2).split("\\|")
      val wordImportance = wordCounts.map(_.split(",")).map { arr =>
        require(
          arr.length >= 2,
          s"Expected at least 2 elements. Found ${arr.mkString("-")} in line"
        )
        (arr.head.stripSuffix("?").stripSuffix("."), arr.last.toDouble / numAnnotators)
      }
      (wordImportance, numAnnotators, question)
    }.groupBy {
      // merging repeated annotations, if they have the same question string
      case (_, _, question) => question
    }.map {
      // merging statistics of the same questions
      case (question, arrayOfCollapsedQuestions) =>
        arrayOfCollapsedQuestions.reduceRight[(Array[(String, Double)], Double, String)] {
          case ((wordImportance, numAnnotator, _), (wordImportanceOverall, numAnnotatorsOverall, _)) =>
            assert(wordImportance.length == wordImportanceOverall.length)
            val wordImportanceMap = wordImportance.toMap
            val mergedWordImportance = wordImportanceOverall.map {
              case (token, importanceOverall) =>
                val totalCount = numAnnotatorsOverall + numAnnotator
                val averageImportance = (importanceOverall * numAnnotatorsOverall +
                  wordImportanceMap(token) * numAnnotator) / totalCount
                (token, averageImportance)
            }
            (mergedWordImportance, numAnnotator + numAnnotatorsOverall, question)
        }
    }.map {
      case (wordImportance, numAnnotators, question) =>
        // logger.info(s"Question: $question // Scores: ${wordImportance.toList}")
        val aristoQuestion = decomposeQuestion(question)
        val essentialTermMap = wordImportance.groupBy(_._1).mapValues(_.maxBy(_._2)._2)
        annotateQuestion(aristoQuestion: Question, Some(essentialTermMap), Some(numAnnotators))
    }.toSeq
    // getting rid of invalid questions
    allQuestions.filter { _.aristoQuestion.selections.nonEmpty }
  }

  def decomposeQuestion(question: String): Question = {
    val maybeSplitQuestion = ParentheticalChoiceIdentifier(question)
    val multipleChoiceSelection = Utils.fallbackDecomposer(maybeSplitQuestion)
    Question(question, Some(maybeSplitQuestion.question),
      multipleChoiceSelection)
  }

  def annotateQuestion(
    aristoQuestion: Question,
    essentialTermMapOpt: Option[Map[String, Double]],
    numAnnotators: Option[Double]
  ): EssentialTermsQuestion = {

    // if the annotation cache already contains it, skip it; otherwise extract the annotation
    val cacheKey = Constants.ANNOTATION_PREFIX + aristoQuestion.text.get + views.asScala.mkString
    val redisAnnotation = redisGet(cacheKey)
    val annotation = if (redisAnnotation.isDefined) {
      SerializationHelper.deserializeFromJson(redisAnnotation.get)
    } else {
      val ta = annotatorService.createAnnotatedTextAnnotation("", "", aristoQuestion.text.get, views)
      ta.getAvailableViews.asScala.foreach { vu =>
        if (ta.getView(vu) == null) {
          logger.error(s">>>>>>>>>>>>>>>>>>>>> ${aristoQuestion.text.get}")
        }
      }
      redisSet(cacheKey, SerializationHelper.serializeToJson(ta))
      ta
    }
    val taWithEssentialTermsView = populateEssentialTermView(annotation, essentialTermMapOpt)
    // logger.info(s"Populated views: ${taWithEssentialTermsView.getAvailableViews.asScala}")

    // salience
    val salienceResultOpt = getSalienceScores(aristoQuestion)

    val (avgSalienceOpt, maxSalienceOpt) = {
      salienceResultOpt match {
        case Some(result) =>
          val listOfMaps = result.map { case (_, salience) => salience.scores }
          // summing the salience scores for different options
          val avgMap = listOfMaps.foldRight(Map[String, Double]()) { (singleMap, combinedMap) =>
            singleMap ++ combinedMap.map { case (k, v) => k -> (v + singleMap.getOrElse(k, 0.0)) }
          }
          // max-ing the salience scores for different options
          val maxMap = listOfMaps.foldRight(Map[String, Double]()) { (singleMap, combinedMap) =>
            val maxMap1 = combinedMap.map {
              case (k, v) =>
                k -> Math.max(v, singleMap.getOrElse(k, v))
            }
            val maxMap2 = singleMap.map {
              case (k, v) =>
                k -> Math.max(v, combinedMap.getOrElse(k, v))
            }
            maxMap1 ++ maxMap2
          }
          (Some(avgMap), Some(maxMap))
        case None => (None, None)
      }
    }
    EssentialTermsQuestion(
      aristoQuestion.rawQuestion,
      essentialTermMapOpt,
      aristoQuestion,
      taWithEssentialTermsView,
      salienceResultOpt,
      avgSalienceOpt,
      maxSalienceOpt,
      numAnnotators
    )
  }

  /** given an aristo question it looks up the salience annotation, either from an static map, or queries salience service */
  def getSalienceScores(q: Question): Option[List[(MultipleChoiceSelection, SalienceResult)]] = {
    val redisSalienceKey = Constants.SALIENCE_PREFIX + q.rawQuestion
    val mapOpt = salienceMap.get(redisSalienceKey)
    // TODO(daniel) move it to application.conf as an option
    val checkForMissingSalienceScores = true
    if (mapOpt.isDefined) {
      //logger.debug("Found the salience score in the static map . . . ")
      mapOpt
    } else if (checkForMissingSalienceScores && q.selections.nonEmpty) {
      val salienceFromRedis = redisGet(redisSalienceKey)
      if (salienceFromRedis.isDefined) {
        logger.debug("Found the salience score in the redis map . . . ")
        Some(salienceFromRedis.get.parseJson.convertTo[List[(MultipleChoiceSelection, SalienceResult)]])
      } else {
        logger.debug(" ===> Caching . . . ")
        logger.debug(q.rawQuestion)
        logger.debug(q.toString)
        val resultFuture = salienceScorer.salienceFor(q)
        val result = Await.result(resultFuture, Duration.Inf)
        val resultJson = result.toList.toJson
        redisSet(redisSalienceKey, resultJson.compactPrint)
        Some(result.toList)
      }
    } else {
      if (!checkForMissingSalienceScores) {
        logger.error("Didn't find the Salience annotation in the cache; if you want to look it up, activate it in your settings . . . ")
        throw new Exception
      } else {
        logger.debug("Question does not have options . . . ")
      }
      logger.debug(q.rawQuestion)
      logger.debug(q.toString)
      None
    }
  }

  // TODO(daniel): you can get rid of this output; right?
  private lazy val annotatorService = {
    val nonDefaultProps = new Properties()
    nonDefaultProps.setProperty(PipelineConfigurator.USE_NER_ONTONOTES.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_SRL_NOM.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_SRL_VERB.key, Configurator.FALSE)
    //    nonDefaultProps.setProperty(PipelineConfigurator.USE_STANFORD_DEP.key, Configurator.FALSE)
    //    nonDefaultProps.setProperty(PipelineConfigurator.USE_STANFORD_PARSE.key, Configurator.FALSE)
    IllinoisPipelineFactory.buildPipeline(
      new CuratorConfigurator().getConfig(new ResourceManager(nonDefaultProps))
    )
  }

  val views = Set(ViewNames.TOKENS, ViewNames.POS, ViewNames.LEMMA, ViewNames.NER_CONLL,
    ViewNames.SHALLOW_PARSE, ViewNames.PARSE_STANFORD, ViewNames.DEPENDENCY_STANFORD).asJava

  // TODO(daniel): move this parameter to application.conf
  val combineNamedEntities = false

  private def populateEssentialTermView(
    ta: TextAnnotation,
    tokenScoreMapOpt: Option[Map[String, Double]]
  ): TextAnnotation = {
    // since the annotated questions have different tokenizations, we first tokenize then assign
    // scores to tokens of spans
    val viewNamesForParsingEssentialTermTokens = if (combineNamedEntities) Set(ViewNames.TOKENS, ViewNames.NER_CONLL) else Set(ViewNames.TOKENS)
    val view = new TokenLabelView(Constants.VIEW_NAME, ta)
    val combinedConsAll = if (combineNamedEntities) {
      getCombinedConsituents(ta)
    } else {
      ta.getView(ViewNames.TOKENS).getConstituents.asScala
    }
    tokenScoreMapOpt match {
      case Some(tokenScoreMap) =>
        val validTokens = tokenScoreMap.flatMap {
          case (tokenString, score) if tokenString.length > 2 => // ignore short spans
            val cacheKey = Constants.TOKENIZATION_PREFIX + tokenString + viewNamesForParsingEssentialTermTokens.toString
            val redisAnnotation = redisGet(cacheKey)
            val tokenTa = if (redisAnnotation.isDefined) {
              SerializationHelper.deserializeFromJson(redisAnnotation.get)
            } else {
              val tokenTaTmp = annotatorService.createAnnotatedTextAnnotation("", "", tokenString,
                viewNamesForParsingEssentialTermTokens.asJava)
              redisSet(cacheKey, SerializationHelper.serializeToJson(ta))
              tokenTaTmp
            }
            val combinedConstituents = if (combineNamedEntities) {
              getCombinedConsituents(tokenTa)
            } else {
              tokenTa.getView(ViewNames.TOKENS).getConstituents.asScala
            }
            val constituents = combinedConstituents.
              filter(_.getSurfaceForm.length > 2) // ignore constituents of short span
            constituents.map(cons => (cons.getSurfaceForm.toLowerCase(), score))
          case _ => List.empty
        }

        combinedConsAll.foreach { cons =>
          if (validTokens.get(cons.getSurfaceForm.toLowerCase()).exists(_ > 0.5)) {
            view.addSpanLabel(
              cons.getStartSpan,
              cons.getEndSpan,
              Constants.IMPORTANT_LABEL,
              validTokens(cons.getSurfaceForm.toLowerCase)
            )
          } else if (!stopWords.contains(cons.getSurfaceForm.toLowerCase())) {
            view.addSpanLabel(
              cons.getStartSpan,
              cons.getEndSpan,
              Constants.UNIMPORTANT_LABEL,
              validTokens.getOrElse(cons.getSurfaceForm.toLowerCase(), 0)
            )
          }
        }
      case None => combinedConsAll.foreach { cons => view.addSpanLabel(cons.getStartSpan, cons.getEndSpan, "", 0) }
    }
    ta.addView(Constants.VIEW_NAME, view)
    ta
  }

  // merges tokens which belong to the same NER constitunet
  def getCombinedConsituents(ta: TextAnnotation): Seq[Constituent] = {
    val tokenConsitutnes = ta.getView(ViewNames.TOKENS).getConstituents.asScala
    val nerConstituents = ta.getView(ViewNames.NER_CONLL).getConstituents.asScala
    tokenConsitutnes.filterNot { c =>
      ta.getView(ViewNames.NER_CONLL).getConstituentsCovering(c).size() > 0
    } ++ nerConstituents
  }

  private def getSentence(qs: Seq[EssentialTermsQuestion]): Iterable[Iterable[Constituent]] = {
    qs.map(
      _.questionTextAnnotation
      .getView(Constants.VIEW_NAME)
      .getConstituents
      .asScala
    )
  }

  def getEssentialTermProbForAristoQuestion(
    aristoQ: Question,
    learner: IllinoisLearner
  ): Map[String, Double] = {
    val questionStruct = annotateQuestion(aristoQ, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) =
      questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.essentialTermTokens.populate(constituents)
    (constituents.map { c => (c.getSurfaceForm, learner.predictProbOfBeingEssential(c)) } ++
      essentialConstituents.map { c => (c.getSurfaceForm, ESSENTIAL_STOPWORD_SCORE) } ++
      nonEssentialConstituents.map { c => (c.getSurfaceForm, NONESSENTIAL_STOPWORD_SCORE) }).toMap
  }

  def getEssentialTermsForAristoQuestion(
    aristoQ: Question,
    learner: IllinoisLearner,
    threshold: Double
  ): Seq[String] = {
    val questionStruct = annotateQuestion(aristoQ, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) =
      questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.essentialTermTokens.populate(constituents)
    constituents.collect { case c if learner.predictIsEssential(c, threshold) => c.getSurfaceForm } ++
      essentialConstituents.map(_.getSurfaceForm)
  }

  def getEssentialTermsForAristoQuestionConstrainedLearner(
    aristoQ: Question,
    dataModel: ExpandedDataModel,
    learner: ConstrainedClassifier[Constituent, Sentence]
  ): Seq[String] = {
    val questionStruct = annotateQuestion(aristoQ, None, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    dataModel.essentialTermTokens.populate(constituents)
    constituents.collect { case c if learner(c) == Constants.IMPORTANT_LABEL => c.getSurfaceForm } ++
      essentialConstituents.map(_.getSurfaceForm)
  }
}
