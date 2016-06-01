package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.edison.features.factory.{ ParseLabelIdentifier, WordFeatureExtractorFactory }
import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, ParentheticalChoiceIdentifier, Question }
import org.allenai.ari.solvers.common.SolversCommonModule
import org.allenai.ari.solvers.common.salience.SalienceScorer
import org.allenai.common.{ Logging, FileUtils }
import org.allenai.common.guice.ActorSystemModule
import org.allenai.datastore.Datastore

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{
  Constituent,
  TextAnnotation,
  TokenLabelView
}
import edu.illinois.cs.cogcomp.core.utilities.configuration.{ Configurator, ResourceManager }
import edu.illinois.cs.cogcomp.core.utilities.{ DummyTextAnnotationGenerator, SerializationHelper }
import edu.illinois.cs.cogcomp.curator.CuratorConfigurator
import edu.illinois.cs.cogcomp.nlp.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.nlp.pipeline.IllinoisPipelineFactory

import akka.actor.ActorSystem
import com.google.inject.Guice
import com.medallia.word2vec.Word2VecModel
import com.redis._
import com.typesafe.config.{ ConfigFactory, ConfigValueFactory }
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector
import spray.json._
import DefaultJsonProtocol._

import scala.collection.JavaConverters._
import scala.io.Codec
import scala.util.Random

import java.io.File
import java.util.Properties

protected case object EssentialTermsConstants {
  val VIEW_NAME = "ESSENTIAL_TERMS"
  val IMPORTANT_LABEL = "IMPORTANT"
  val UNIMPORTANT_LABEL = "NOT-IMPORTANT"
  val LABEL_SEPARATOR = "*|*|*"
}

object EssentialTermsSensors extends Logging {

  lazy val allQuestions = readAndAnnotateEssentialTermsData()

  lazy val stopWords = {
    lazy val stopWordsFile = EssentialTermsUtils.getDatastoreFileAsSource(
      "public", "org.allenai.termselector", "stopwords.txt", 1
    )
    val stopWords = stopWordsFile.getLines().toList
    stopWordsFile.close()
    stopWords.toSet ++ Set("__________")
  }
  // since we never to learning on the above stopwords, we choose a subset of the stopwords to ALWAYS be essential
  lazy val essentialStopWords = Set("all", "any", "because", "before", "both", "but")
  val ESSENTIAL_STOPWORD_SCORE = 0.8
  lazy val nonessentialStopWords = stopWords.--(essentialStopWords)
  val NONESSENTIAL_STOPWORD_SCORE = 0.0

  // a hashmap from sentences to [[TextAnnotation]]s
  // TODO(daniel): this is not used currently; decide if you want to use this file, or use redis
  lazy val annotationFileCache = {
    val annotationCacheFile = EssentialTermsUtils.getDatastoreFileAsSource(
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

  // TODO(danm): This path should come from config so anyone can run the code.
  private lazy val word2vecFile = new File(
    "/Users/daniel/ideaProjects/convertvec/GoogleNews-vectors-negative300-length=200000.bin"
  )
  lazy val w2vModel = Word2VecModel.fromBinFile(word2vecFile)
  lazy val w2vNoMatchStr = "</s>" // string used by word2vec when there is no match

  private lazy val salienceMap = {
    val salienceCache = EssentialTermsUtils.getDatastoreFileAsSource(
      "public", "org.allenai.termselector", "salienceCache.txt", 1
    )
    val lines = salienceCache.getLines
    val cache = lines.grouped(2).map {
      case q :: json :: _ =>
        q -> json.parseJson.convertTo[List[(MultipleChoiceSelection, SalienceResult)]]
    }.toMap
    salienceCache.close()
    cache
  }

  lazy val (trainConstiuents, testConstituents, trainSentences, testSentences) = {
    val trainProb = 0.7
    // in order to be consistent across runs
    Random.setSeed(10)
    val (train, test) = allQuestions.partition(_ => Random.nextDouble() < trainProb)
    val trainSen = getSentence(train)
    val testSen = getSentence(test)

    // add a train attribute to the training constituents, in order to make sure they will have
    // different hashcode than the test constituents
    trainSen.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("trainidx", s"$idx") }
    testSen.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("testidx", s"${9999 + idx}") }
    (trainSen.flatten, testSen.flatten, trainSen, testSen)
  }

  lazy val allConstituents = trainConstiuents ++ testConstituents

  lazy val allSentences = trainSentences ++ testSentences

  // This creates a map from constituents, to its corresponding [[QuestionStruct]] which contains
  // the annotations of the question containing it.
  // TODO: make this immutable
  lazy val constituentToAnnotationMap = collection.mutable.Map(allQuestions.flatMap { q =>
    val constituents =
      q.questionTextAnnotation
        .getView(EssentialTermsConstants.VIEW_NAME)
        .getConstituents
        .asScala
    constituents.map(_ -> q)
  }: _*)

  lazy val (salienceScorer, actorSystem) = {
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
    val file = EssentialTermsUtils.getDatastoreFileAsSource(datastoreName, group, name, version)
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
      case (wordImportance, _, question) =>
        // logger.info(s"Question: $question // Scores: ${wordImportance.toList}")
        val maybeSplitQuestion = ParentheticalChoiceIdentifier(question)
        val multipleChoiceSelection = EssentialTermsUtils.fallbackDecomposer(maybeSplitQuestion)
        val aristoQuestion = Question(question, Some(maybeSplitQuestion.question),
          multipleChoiceSelection)
        val essentialTermMap = wordImportance.groupBy(_._1).mapValues(_.maxBy(_._2)._2)
        annotateQuestion(aristoQuestion: Question, Some(essentialTermMap))
    }.toSeq
    // getting rid of invalid questions
    allQuestions.filter { _.aristoQuestion.selections.nonEmpty }
  }

  def annotateQuestion(
    aristoQuestion: Question,
    essentialTermMapOpt: Option[Map[String, Double]]
  ): EssentialTermsQuestion = {

    // if the annotation cache already contains it, skip it; otherwise extract the annotation
    val cacheKey = aristoQuestion.text.get + views.asScala.mkString
    val redisAnnotation = annotationRedisCache.get(cacheKey)
    val annotation = if (redisAnnotation.isDefined) {
      SerializationHelper.deserializeFromJson(redisAnnotation.get)
    } else {
      val ta = annotatorService.createAnnotatedTextAnnotation("", "", aristoQuestion.text.get, views)
      ta.getAvailableViews.asScala.foreach { vu =>
        if (ta.getView(vu) == null) {
          logger.error(s">>>>>>>>>>>>>>>>>>>>> ${aristoQuestion.text.get}")
        }
      }
      annotationRedisCache.set(cacheKey, SerializationHelper.serializeToJson(ta))
      ta
    }
    val taWithEssentialTermsView = populateEssentialTermView(annotation, essentialTermMapOpt)
    // logger.info(s"Populated views: ${taWithEssentialTermsView.getAvailableViews.asScala}")

    // salience
    val salienceResultOpt = salienceMap.get(aristoQuestion.rawQuestion)
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
      maxSalienceOpt
    )
  }

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

  private def populateEssentialTermView(
    ta: TextAnnotation,
    tokenScoreMapOpt: Option[Map[String, Double]]
  ): TextAnnotation = {
    // since the annotated questions have different tokenizations, we first tokenize then asign essentiality scores
    // to tokens of spans
    val view = new TokenLabelView(EssentialTermsConstants.VIEW_NAME, ta)
    tokenScoreMapOpt match {
      case Some(tokenScoreMap) =>
        val validTokens = tokenScoreMap.flatMap {
          case (tokenString, score) if tokenString.length > 2 => // ignore short spans
            val cacheKey = "**essentialTermTokenization:" + tokenString
            val redisAnnotation = annotationRedisCache.get(cacheKey)
            val ta = if (redisAnnotation.isDefined) {
              SerializationHelper.deserializeFromJson(redisAnnotation.get)
            } else {
              val ta = annotatorService.createAnnotatedTextAnnotation("", "", tokenString,
                Set(ViewNames.TOKENS).asJava)
              annotationRedisCache.set(cacheKey, SerializationHelper.serializeToJson(ta))
              ta
            }
            val constituents = ta.getView(ViewNames.TOKENS).getConstituents.asScala
              .filter(_.getSurfaceForm.length > 2) // ignore constituents of short span
            constituents.map(cons => (cons.getSurfaceForm.toLowerCase(), score))
          case _ => List.empty
        }

        ta.getView(ViewNames.TOKENS).getConstituents.asScala.foreach { cons =>
          if (validTokens.get(cons.getSurfaceForm.toLowerCase()).exists(_ > 0.5)) {
            view.addSpanLabel(
              cons.getStartSpan,
              cons.getEndSpan,
              EssentialTermsConstants.IMPORTANT_LABEL,
              validTokens(cons.getSurfaceForm.toLowerCase)
            )
          } else if (!stopWords.contains(cons.getSurfaceForm.toLowerCase())) {
            view.addSpanLabel(
              cons.getStartSpan,
              cons.getEndSpan,
              EssentialTermsConstants.UNIMPORTANT_LABEL,
              -1
            )
          }
        }
      case None =>
        ta.getView(ViewNames.TOKENS).getConstituents.asScala.foreach { cons =>
          view.addSpanLabel(cons.getStartSpan, cons.getEndSpan, "", -1)
        }
    }
    ta.addView(EssentialTermsConstants.VIEW_NAME, view)
    ta
  }

  private def getSentence(qs: Seq[EssentialTermsQuestion]): Iterable[Iterable[Constituent]] = {
    qs.map(
      _.questionTextAnnotation
      .getView(EssentialTermsConstants.VIEW_NAME)
      .getConstituents
      .asScala
    )
  }

  def getEssentialTermProbForAristoQuestion(
    aristoQ: Question,
    learner: IllinoisLearner
  ): Map[String, Double] = {
    val questionStruct = annotateQuestion(aristoQ, None)
    val (stopwordConstituents, constituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.essentialTermTokens.populate(constituents)
    (constituents.map { c => (c.getSurfaceForm, learner.predictProbOfBeingEssential(c)) } ++
      essentialConstituents.map { c => (c.getSurfaceForm, ESSENTIAL_STOPWORD_SCORE) } ++
      nonEssentialConstituents.map { c => (c.getSurfaceForm, NONESSENTIAL_STOPWORD_SCORE) }).toMap
  }

  def getEssentialTermsForAristoQuestion(
    aristoQ: Question,
    learner: IllinoisLearner
  ): Seq[String] = {
    val questionStruct = annotateQuestion(aristoQ, None)
    val (constituents, stopwordConstituents) = questionStruct.getConstituents(stopWords)
    val (essentialConstituents, nonEssentialConstituents) = questionStruct.getConstituents(stopwordConstituents, essentialStopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.essentialTermTokens.populate(constituents)
    constituents.collect { case c if learner.predictIsEssential(c) => c.getSurfaceForm } ++
      essentialConstituents.map(_.getSurfaceForm)
  }
}
