package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, Question }
import org.allenai.common.{ FileUtils, Logging }
import org.allenai.datastore.Datastore

import com.redis.RedisClient
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, TextAnnotation, TokenLabelView }
import edu.illinois.cs.cogcomp.core.utilities.configuration.{ Configurator, ResourceManager }
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.curator.CuratorConfigurator
import edu.illinois.cs.cogcomp.nlp.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.nlp.pipeline.IllinoisPipelineFactory
import spray.json._
import DefaultJsonProtocol._

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Codec
import java.util.Properties

object Annotations extends Logging {
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

  def populateEssentialTermView(
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
          } else if (!EssentialTermsSensors.stopWords.contains(cons.getSurfaceForm.toLowerCase())) {
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

  /** given an aristo question it looks up the salience annotation, either from an static map, or queries salience service */
  def getSalienceScores(q: Question): Option[List[(MultipleChoiceSelection, SalienceResult)]] = {
    val redisSalienceKey = Constants.SALIENCE_PREFIX + q.rawQuestion
    val mapOpt = EssentialTermsSensors.salienceMap.get(redisSalienceKey)
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
        val resultFuture = EssentialTermsSensors.salienceScorer.salienceFor(q)
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

  def readAndAnnotateEssentialTermsData(): Seq[EssentialTermsQuestion] = {
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
        val aristoQuestion = Utils.decomposeQuestion(question)
        val essentialTermMap = wordImportance.groupBy(_._1).mapValues(_.maxBy(_._2)._2)
        annotateQuestion(aristoQuestion: Question, Some(essentialTermMap), Some(numAnnotators))
    }.toSeq
    // getting rid of invalid questions
    allQuestions.filter { _.aristoQuestion.selections.nonEmpty }
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

  // merges tokens which belong to the same NER constitunet
  def getCombinedConsituents(ta: TextAnnotation): Seq[Constituent] = {
    val tokenConsitutnes = ta.getView(ViewNames.TOKENS).getConstituents.asScala
    val nerConstituents = ta.getView(ViewNames.NER_CONLL).getConstituents.asScala
    tokenConsitutnes.filterNot { c =>
      ta.getView(ViewNames.NER_CONLL).getConstituentsCovering(c).size() > 0
    } ++ nerConstituents
  }

  def getSentence(qs: Seq[EssentialTermsQuestion]): Iterable[Iterable[Constituent]] = {
    qs.map(
      _.questionTextAnnotation
      .getView(Constants.VIEW_NAME)
      .getConstituents
      .asScala
    )
  }

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

}
