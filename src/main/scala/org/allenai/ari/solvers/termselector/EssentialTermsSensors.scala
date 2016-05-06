package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, ParentheticalChoiceIdentifier, Question }
import org.allenai.ari.solvers.common.SolversCommonModule
import org.allenai.ari.solvers.common.salience.SalienceScorer
import org.allenai.ari.solvers.termselector.EssentialTermsUtils.Levenshtein
import org.allenai.common.FileUtils

import org.allenai.common.guice.ActorSystemModule
import org.allenai.datastore.Datastore

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{
  Constituent,
  TextAnnotation,
  TokenLabelView
}
import edu.illinois.cs.cogcomp.core.utilities.configuration.{ Configurator, ResourceManager }
import edu.illinois.cs.cogcomp.curator.CuratorConfigurator
import edu.illinois.cs.cogcomp.nlp.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.nlp.pipeline.IllinoisPipelineFactory

import akka.actor.ActorSystem
import com.google.inject.Guice
import com.medallia.word2vec.Word2VecModel
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
}

object EssentialTermsSensors {

  lazy val allQuestions = readAndAnnotateEssentialTermsData()

  // a convenient way to access essential term scores, given a question surface form
  lazy val questionEssentialTermScores = allQuestions.map { a => a.rawQuestion -> a.essentialTermMap }

  lazy val stopWords = {
    lazy val stopWordsFile = EssentialTermsUtils.getDatastoreFileAsSource(
      "public", "org.allenai.termselector", "stopwords.txt", 1
    )
    val stopWords = stopWordsFile.getLines().toList
    stopWordsFile.close()
    stopWords.toSet
  }

  private lazy val word2vecFile = new File("/Users/daniel/ideaProjects/convertvec/GoogleNews-vectors-negative300-length=200000.bin")
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
    // just to make sure across runs we are consistent
    Random.setSeed(10)
    val (train, test) = allQuestions.partition(_ => Random.nextDouble() < trainProb)
    val trainSen = getSentence(train)
    val testSen = getSentence(test)
    // add a train attribute to the training constituents, in order to make sure they will have different hashcode than
    // the test constituents
    trainSen.flatten.foreach(_.addAttribute("train", "true"))
    (trainSen.flatten, testSen.flatten, trainSen, testSen)
  }

  lazy val allConstituents = trainConstiuents ++ testConstituents

  lazy val allSentences = trainSentences ++ testSentences

  // This creates a map from constituents, to its corresponding [[QuestionStruct]] which contains
  // the annotations of the question containing it.
  // TODO: make this immutable
  lazy val constituentToAnnotationMap = collection.mutable.Map(allQuestions.flatMap { q =>
    val constituents = q.questionTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala
    constituents.map(_ -> q)
  }: _*)

  lazy val (salienceScorer, actorSystem) = {
    implicit val system = ActorSystem("ari-http-solver")
    val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
    val localConfig = rootConfig.getConfig("ari.solvers.common").withValue(
      "wumpus-overrides",
      ConfigValueFactory.fromMap(Map("redisTimeoutMillis" -> Int.MaxValue).asJava)
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

  def getConstituentAfter(x: Constituent): Constituent = {
    val consAfter = x.getView.getConstituents.asScala.
      filter(cons => cons.getStartSpan >= x.getEndSpan)
    if (consAfter.nonEmpty) consAfter.minBy(_.getEndSpan) else x
  }

  def getConstituentBefore(x: Constituent): Constituent = {
    val consBefore = x.getView.getConstituents.asScala.
      filter(cons => cons.getEndSpan <= x.getStartSpan)
    if (consBefore.nonEmpty) consBefore.maxBy(_.getEndSpan) else x
  }

  def getConstituentTwoAfter(x: Constituent): Constituent = {
    val consAfter = x.getView.getConstituents.asScala.
      filter(cons => cons.getStartSpan >= x.getEndSpan)
    if (consAfter.size >= 2) consAfter.sortBy(_.getEndSpan).toList(1) else x
  }

  def getConstituentTwoBefore(x: Constituent): Constituent = {
    val consBefore = x.getView.getConstituents.asScala.
      filter(cons => cons.getEndSpan <= x.getStartSpan)
    if (consBefore.size >= 2) consBefore.sortBy(-_.getEndSpan).toList(1) else x
  }

  def getConstituentCoveringInView(c: Constituent, view: String): java.util.List[Constituent] = {
    c.getTextAnnotation.getView(view).getConstituentsCovering(c)
  }

  private def readAndAnnotateEssentialTermsData(): Seq[EssentialTermsQuestion] = {
    val salientTermsFile = Datastore("private").filePath(
      "org.allenai.termselector", "turkerSalientTerms.tsv", 1
    )
    // Some terms in the turker generated file need ISO-8859 encoding
    val allQuestions = FileUtils.getFileAsLines(salientTermsFile.toFile)(Codec.ISO8859).map {
      line =>
        val fields = line.split("\t")
        assert(fields.size == 3, "Expected format: question numAnnotators word-counts. Got: " + line)
        val question = fields(0)
        val numAnnotators = fields(1).toDouble
        val wordCounts = fields(2).split("\\|")
        val wordImportance = wordCounts.map(_.split(",")).map { arr =>
          assert(arr.size >= 2, "Expected at least elements. Found: " + arr.mkString("-") + " in " +
            line)
          (arr.head.stripSuffix("?").stripSuffix("."), arr.last.toDouble / numAnnotators)
        }

        val maybeSplitQuestion = ParentheticalChoiceIdentifier(question)
        val multipleChoiceSelection = EssentialTermsUtils.fallbackDecomposer(maybeSplitQuestion)
        val aristoQuestion = Question(question, Some(maybeSplitQuestion.question), multipleChoiceSelection)
        val essentialTermMap = wordImportance.groupBy(_._1).mapValues(_.maxBy(_._2)._2)
        annotateQuestion(aristoQuestion: Question, Some(essentialTermMap))
    }
    // getting rid of invalid questions
    allQuestions.filter { _.aristoQuestion.selections.nonEmpty }
  }

  def annotateQuestion(aristoQuestion: Question, essentialTermMapOpt: Option[Map[String, Double]]): EssentialTermsQuestion = {
    val ta = annotatorService.createAnnotatedTextAnnotation("", "", aristoQuestion.text.get, views)
    val taWithEssentialTermsView = populateEssentialTermView(ta, essentialTermMapOpt)

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
            val maxMap1 = combinedMap.map { case (k, v) => k -> Math.max(v, singleMap.getOrElse(k, v)) }
            val maxMap2 = singleMap.map { case (k, v) => k -> Math.max(v, combinedMap.getOrElse(k, v)) }
            maxMap1 ++ maxMap2
          }
          (Some(avgMap), Some(maxMap))
        case None => (None, None)
      }
    }
    EssentialTermsQuestion(aristoQuestion.rawQuestion, essentialTermMapOpt, aristoQuestion, taWithEssentialTermsView,
      salienceResultOpt, avgSalienceOpt, maxSalienceOpt)
  }

  private lazy val annotatorService = {
    val nonDefaultProps = new Properties()
    nonDefaultProps.setProperty(PipelineConfigurator.USE_POS.key, Configurator.TRUE)
    //    nonDefaultProps.setProperty(PipelineConfigurator.USE_NER_CONLL.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_NER_ONTONOTES.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_SRL_VERB.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_SRL_NOM.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_STANFORD_DEP.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_STANFORD_PARSE.key, Configurator.FALSE)
    nonDefaultProps.setProperty(PipelineConfigurator.USE_SHALLOW_PARSE.key, Configurator.FALSE)
    IllinoisPipelineFactory.buildPipeline(new CuratorConfigurator().getConfig(new ResourceManager(nonDefaultProps)))
  }

  val views = Set(ViewNames.TOKENS, ViewNames.POS, ViewNames.LEMMA, ViewNames.NER_CONLL).asJava //,
  //      ViewNames.DEPENDENCY_STANFORD, ViewNames.PARSE_STANFORD).asJava
  //, ViewNames.SRL_VERB, ).asJava
  // ViewNames.CHUNK, ViewNames.NER_CONLL

  private def populateEssentialTermView(ta: TextAnnotation, tokenScoreMapOpt: Option[Map[String, Double]]): TextAnnotation = {
    val view = new TokenLabelView(EssentialTermsConstants.VIEW_NAME, ta)
    tokenScoreMapOpt match {
      case Some(tokenScoreMap) =>
        val validTokens = tokenScoreMap.flatMap {
          case (tokenString, score) if tokenString.length > 2 =>
            val ta = annotatorService.createAnnotatedTextAnnotation("", "", tokenString,
              Set(ViewNames.TOKENS).asJava)
            val constituents = ta.getView(ViewNames.TOKENS).getConstituents.asScala
              .filter(_.getSurfaceForm.length > 2)
            constituents.map(cons => (cons.getSurfaceForm.toLowerCase(), score))
          case _ => List.empty
        }

        ta.getView(ViewNames.TOKENS).getConstituents.asScala.foreach { cons =>
          if (validTokens.get(cons.getSurfaceForm.toLowerCase()).exists(_ > 0.9)) {
            view.addSpanLabel(cons.getStartSpan, cons.getEndSpan, EssentialTermsConstants.IMPORTANT_LABEL,
              validTokens(cons.getSurfaceForm.toLowerCase))
          } else if (!stopWords.contains(cons.getSurfaceForm.toLowerCase())) {
            view.addSpanLabel(cons.getStartSpan, cons.getEndSpan, EssentialTermsConstants.UNIMPORTANT_LABEL, -1)
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
    qs.map(_.questionTextAnnotation.getView(EssentialTermsConstants.VIEW_NAME).getConstituents.asScala)
  }

  def getEssentialTermProbForAristoQuestion(
    aristoQ: Question,
    learner: EssentialTermsLearner
  ): Map[Constituent, Double] = {
    val questionStruct = annotateQuestion(aristoQ, None)
    val constituents = questionStruct.getConstituents(stopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.tokens.populate(constituents)
    constituents.map { c => (c, learner.predictProbOfBeingEssential(c)) }.toMap
  }

  def getEssentialTermsForAristoQuestion(
    aristoQ: Question,
    learner: EssentialTermsLearner
  ): Seq[Constituent] = {
    val questionStruct = annotateQuestion(aristoQ, None)
    val constituents = questionStruct.getConstituents(stopWords)
    // update the inverse map with the new constituents
    constituents.foreach(c => constituentToAnnotationMap.put(c, questionStruct))
    learner.dataModel.tokens.populate(constituents)
    constituents.filter(learner.predictIsEssential)
  }

  /** a convenient function to access the mturk annotated data, given an aristo question */
  def getMturkEssentialTermsScoresForAristoQuestion(aristoQ: Question): Option[Map[String, Double]] = {
    val (_, termMap, minDistance) = questionEssentialTermScores.map {
      case (q, termScores) =>
        (q, termScores, Levenshtein.distance(q, aristoQ.rawQuestion))
    }.minBy { case (_, _, distance) => distance }
    val DISTANCE_THRESHOLD = 5
    if (minDistance < DISTANCE_THRESHOLD) {
      termMap
    } else {
      throw new Exception("Annotation not found for question: " + aristoQ)
    }
  }
}
