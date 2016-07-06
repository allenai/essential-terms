package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.MultipleChoiceSelection
import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.solvers.common.SolversCommonModule
import org.allenai.ari.solvers.common.salience.SalienceScorer
import org.allenai.common.{ FileUtils, Logging }
import org.allenai.common.guice.ActorSystemModule
import org.allenai.datastore.Datastore

import ch.qos.logback.classic.Level
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.edison.features.factory.WordFeatureExtractorFactory
import akka.actor.ActorSystem
import com.google.inject.Guice
import com.typesafe.config.{ ConfigFactory, ConfigValueFactory }
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector
import spray.json._
import DefaultJsonProtocol._
import scala.collection.JavaConverters._
import scala.util.Random

/** The purpose of this object is to contain the entry points (hence "sensors") to all
  * the datasets and resources used throughout the project.
  */
object Sensors extends Logging {
  // reading the config file
  private val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)
  val localConfig = rootConfig.getConfig("ari.solvers.termselector")

  // the set of the questions annotated with mechanical turk
  lazy val allQuestions = Annotator.readAndAnnotateEssentialTermsData()

  lazy val stopWords = {
    val stopWordsFile = Utils.getDatastoreFileAsSource(
      localConfig.getString("stopwordsDatastoreFile")
    )
    val stopWords = stopWordsFile.getLines().toList
    stopWordsFile.close()
    (stopWords :+ "__________").toSet
  }
  lazy val nonessentialStopWords = stopWords.diff(Constants.essentialStopWords)

  // salience, used when the annotation does not exist in our cache
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

  // the salience cache for regents, regents-test++ and omnibus, for faster evaluation
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
    questions.map { q => Utils.decomposeQuestion(separator.replaceAllIn(q, " ").replaceAll("\"", "")).text }
  }

  // splitting the data into test and train
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
    val trainSentences = Annotator.getConstituents(train)
    val testSentences = Annotator.getConstituents(test)
    val devSentences = Annotator.getConstituents(dev)

    val filterMidScoreConsitutents = localConfig.getDoubleList("annotation.filterMidScoreConsitutents").asScala
    val filteredTrainSen = if (filterMidScoreConsitutents.nonEmpty) {
      require(filterMidScoreConsitutents.length == 2, "The parameter \"filterMidScoreConsitutents\" " +
        "should be an real-array of length 2.")
      trainSentences.map { consList =>
        consList.toList.filter { c =>
          c.getConstituentScore >= filterMidScoreConsitutents(0) ||
            c.getConstituentScore <= filterMidScoreConsitutents(1)
        }
      }
    } else {
      trainSentences
    }

    // add a train attribute to the training constituents, in order to make sure they will have
    // different hashcode than the test constituents
    trainSentences.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("trainidx", s"$idx") }
    testSentences.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("testidx", s"${9999 + idx}") }
    devSentences.flatten.zipWithIndex.foreach { case (c, idx) => c.addAttribute("devidx", s"${999999 + idx}") }
    (filteredTrainSen.flatten, testSentences.flatten, devSentences.flatten, filteredTrainSen, testSentences, devSentences)
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

  /** This creates a map from constituents, to its corresponding [[EssentialTermsQuestion]] which contains
    * the annotations of the question containing it.
    */
  // TODO(daniel): we might consider making this immutable; although it might make things complicated
  lazy val constituentToAnnotationMap = collection.mutable.Map(allQuestions.flatMap { q =>
    val constituents =
      q.questionTextAnnotation
        .getView(Constants.VIEW_NAME)
        .getConstituents
        .asScala
    constituents.map(_ -> q)
  }: _*)

  /** Load science terms from Datastore */
  lazy val scienceTerms: Set[String] = {
    val file = Utils.getDatastoreFileAsSource(localConfig.getString("scienceTermsDatastoreFile"))
    val terms = file.getLines().filterNot(_.startsWith("#")).toSet
    file.close()
    terms
  }

  val brownClusterFeatureExtractor = WordFeatureExtractorFactory.getBrownFeatureGenerator(
    "",
    "brown-clusters/brown-rcv1.clean.tokenized-CoNLL03.txt-c100-freq1.txt", Array[Int](4, 5)
  )
}
