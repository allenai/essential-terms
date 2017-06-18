package org.allenai.ari.solvers.common.salience

import java.io.File

import akka.actor.ActorSystem
import com.google.inject.name.Named
import com.google.inject.{Inject, Singleton}
import com.typesafe.config.Config
import org.allenai.ari.models.{MultipleChoiceSelection, Question}
import org.allenai.ari.models.salience.{PmiResult, SalienceResult}
import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.common.Config._
import org.allenai.datastore.Datastore
import org.allenai.wumpus.client.WumpusClient
import spray.caching.{Cache, LruCache}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object SalienceScorer {
  /** Based upon given config, return either an instance of the SalienceScorer wrapped in Some,
    * or None if salience scores are disabled.
    * Construct the scorer in part by building a `WumpusClient` using the given config.
    * @param config top-level controller config
    */
  def fromConfig(config: Config)(implicit actorSystem: ActorSystem): Option[SalienceScorer] = {

    val salienceConfig: Config = config[Config]("ari.controller.salience")
    if (salienceConfig[Boolean]("addSalienceScores")) {

      val smartStopwords: Set[String] = {
        val stoplistPath = Datastore.filePath(
          salienceConfig[String]("stoplist.group"),
          salienceConfig[String]("stoplist.name"),
          salienceConfig[Int]("stoplist.version")
        )
        val stoplistFile = new File(stoplistPath.toString)
        Source.fromFile(stoplistFile).getLines().toSet
      }
      val tokenizer = new KeywordTokenizer(smartStopwords, Map.empty)

      val wumpusClient = {
        val wumpusConfig: Config = salienceConfig[Config]("wumpusClient")
        WumpusClient.instance(
          hostname = wumpusConfig[String]("host"),
          port = wumpusConfig[Int]("port"),
          numConnections = wumpusConfig[Int]("numConnections"),
          timeoutMillis = wumpusConfig[Int]("timeoutMillis"),
          normalizeWhitespace = wumpusConfig[Boolean]("normalizeWhitespace"),
          redisConfig = wumpusConfig[Config]("redis")
        )
      }

      val salienceCacheSize = salienceConfig
        .get[Int]("salience.cacheSize")
        .getOrElse(0)

      Some(new SalienceScorer(wumpusClient, tokenizer, salienceCacheSize)(actorSystem.dispatcher))
    } else {
      None
    }
  }
}

/** A utility class to calculate salience scores for a question with answer options.
  *
  * @constructor create a scorer
  * @param wumpusClient an instance of WumpusClient
  * @param tokenizer the KeywordTokenizer used for creating ngrams
  * @param salienceCacheSize the size of the local LRU cache of Wumpus count query results. If
  * <=0 then no cache will be used.
  */
@Singleton class SalienceScorer @Inject() (
    wumpusClient: WumpusClient,
    tokenizer: KeywordTokenizer,
    @Named("salience.cacheSize") salienceCacheSize: Int
)(implicit ec: ExecutionContext) {

  // A regular expression for identifying purely alphabetical tokens
  val alphabetic = """^[\p{IsAlphabetic}]*$""".r

  /** When calculating PMI for question-answer pairs, it is possible (even likely) that duplicate
    * "count" queries will be made to the Wumpus client in close enough succession that they'll
    * all be cache misses. To compensate for that we maintain a LRU cache of counts that
    * ensures that any given count query will only hit Wumpus once (until it falls out of the
    * cache)
    */
  val localCache: Option[Cache[Long]] =
    if (salienceCacheSize > 0) Some(LruCache(maxCapacity = salienceCacheSize)) else None

  // Get the count by first checking the local cache, then falling back to making a wumpus call.
  def wumpusCount(query: String) = localCache match {
    case None => wumpusClient.count(query)
    case Some(cache) => cache(query) { wumpusClient.count(query) }
  }

  /** @return the map of answer option to salience result for the given question */
  def salienceFor(
    question: Question
  ): Future[Map[MultipleChoiceSelection, SalienceResult]] = {
    require(question.text.nonEmpty, "Salience requires properly-parsed questions")
    val textWithoutAnswers = question.text.get

    // --------------------------------------------------------------------------------
    // NOTE: If an answer option does not contain any content words (that is, the
    //       answer option is all stopwords or non-alphabetic characters or symbols),
    //       then this function may return an empty map (Future[Map[]]). Solvers that
    //       use the Salience Scorer must be prepared to receive an empty map.
    // --------------------------------------------------------------------------------

    // Salience Scorer - Experiment 5K
    //
    // - generates salience scores for words in a question-answer pair
    // - generates PMI scores for patterns in a question-answer pair
    //
    // - patterns are unigram, bigrams, trigrams, and skip bigrams
    //
    // - algorithm is from Experiment 5K in the AI2 folder in Google Drive:
    //
    //      My Drive > AI2 > Aristo > Salience
    //      "Salience and Co-occurrence Experiments"

    // Step 1: generate skip bigrams ("X ... Y" patterns)
    // --------------------------------------------------

    // Score each question unigram by its maximum PMI with any answer ngram.
    // Select the top N highest scoring unigrams.
    // Make all possible N*(N-1)/2 pairs of the top N unigrams and add these
    // as new features.

    // Question unigrams
    val qUnigrams = uniGrams(textWithoutAnswers)
    // Sequence of max PMI values for all question unigrams.
    val maxPmisFuture: Future[Seq[Double]] = Future.sequence {
      qUnigrams map { qPattern =>
        val questionPmis: Seq[Future[Double]] = for {
          selection <- question.selections
          answer = selection.answer.getOrElse(selection.focus)
          answerPatterns = nGrams(answer)
          aPattern <- answerPatterns
        } yield newPmi(qPattern, aPattern)
        // Calculate the max for each!
        Future.sequence(questionPmis) map { qPmis =>
          if (qPmis.nonEmpty) qPmis.max else 0d
        }
      }
    }
    // Sort the unigrams by PMI.
    val sortedUniFuture: Future[Seq[String]] = maxPmisFuture map { maxPmis =>
      (qUnigrams zip maxPmis sortBy { -_._2 }).unzip._1
    }

    // Generate skip bigrams, restricting the component unigrams to the topN unigrams
    val topN = 5
    val qSkipBigramsFuture: Future[Seq[String]] = sortedUniFuture map { sortedUni =>
      skipBigrams(textWithoutAnswers, sortedUni.take(topN))
    }

    // Step 2: generate unigrams, bigrams, trigrams, and mix together with skip bigrams
    // --------------------------------------------------------------------------------

    // Question n-grams and pair sequences
    val questionPatternsFuture: Future[Seq[String]] = qSkipBigramsFuture map { qSkipBigrams =>
      val qNGrams = nGrams(textWithoutAnswers)
      qNGrams ++ qSkipBigrams
    }

    // Step 3: calculate PMI scores for n-grams and skip bigrams
    // ---------------------------------------------------------

    // PMI results, one per selection.
    val allPmiResultsFuture: Future[Seq[Seq[PmiResult]]] = {
      questionPatternsFuture flatMap { questionPatterns =>
        // Calculate PMI for each <questionPattern, answerPattern> pair.
        val pmiResultsFuture: Seq[Future[Seq[PmiResult]]] = question.selections map { selection =>
          val answer = selection.answer.getOrElse(selection.focus)
          val answerNGrams = nGrams(answer)
          val answerUnigrams = uniGrams(answer)
          val answerSPairs = skipBigrams(answer, answerUnigrams)
          val answerPatterns = answerNGrams ++ answerSPairs
          val distinctPairs = (for {
            aPattern <- answerPatterns
            qPattern <- questionPatterns
          } yield (aPattern, qPattern)).distinct
          val resultsForSelection: Seq[Future[PmiResult]] = for {
            (aPattern, qPattern) <- distinctPairs
          } yield newPmi(qPattern, aPattern) map { pmiValue =>
            PmiResult(qPattern, aPattern, pmiValue)
          }
          Future.sequence(resultsForSelection)
        }
        Future.sequence(pmiResultsFuture)
      }
    }

    // Step 4: calculate salience scores for tokens in the question-answer pairs
    // -------------------------------------------------------------------------

    // For each selection, calculate the contribution of each token to the final
    // score for the selection
    val salienceResultsFuture: Future[Map[MultipleChoiceSelection, SalienceResult]] = {
      allPmiResultsFuture map { allPmiResults =>
        val selToken2Contrib = mutable.Map.empty[String, Double]
        for {
          (selection, results) <- question.selections zip allPmiResults
          PmiResult(qPattern, aPattern, pmiValue) <- results
          tokens = (uniGrams(qPattern) ++ uniGrams(aPattern)).distinct // unique tokens
          token <- tokens
          selToken = selection.key + "|" + token
          sampleSize = results.size * tokens.size
        } {
          val newContrib = pmiValue / sampleSize
          if (!selToken2Contrib.contains(selToken)) { // initialize hash
            selToken2Contrib(selToken) = newContrib
          } else { // update hash
            // sum of contributions
            selToken2Contrib(selToken) = selToken2Contrib(selToken) + newContrib
          }
        }

        // For each selection, let the salience of each token be its contribution
        // to the final score for that selection.
        val salResults = for {
          (selection, pmiValues) <- question.selections zip allPmiResults
          answer = selection.answer.getOrElse(selection.focus)
          aUnigrams = uniGrams(answer)
          if aUnigrams.nonEmpty
        } yield {
          val tokenContribs = for {
            token <- (qUnigrams ++ aUnigrams).distinct // unique tokens
            selToken = selection.key + "|" + token
            contrib = selToken2Contrib(selToken)
          } yield token -> contrib
          val salienceValues: Map[String, Double] = tokenContribs.toMap
          selection -> SalienceResult(salienceValues, pmiValues)
        }
        salResults.toMap
      }
    }

    // Step 5: return the PMI scores and the Salience scores
    // -----------------------------------------------------

    salienceResultsFuture
  }

  /** Generate skip bigrams -- "X ... Y"
    */
  def skipBigrams(originalString: String, topUnigrams: Seq[String]): Seq[String] = {
    val orderedUnigrams = uniGrams(originalString).toIndexedSeq
    // Make all possible pairs of the top N
    // Sort a pair (A,B) so that A occurs before B in the original question
    for {
      i <- 0 until (orderedUnigrams.length - 1)
      word1 = orderedUnigrams(i)
      if topUnigrams.contains(word1)
      j <- (i + 1) until orderedUnigrams.length
      word2 = orderedUnigrams(j)
      if topUnigrams.contains(word2)
      if !ngramOverlap(word1, word2)
    } yield word1 + " ... " + word2
  }

  /** Extract unigrams, bigrams, and trigrams
    */
  def nGrams(rawText: String): Seq[String] = {
    val unigrams = uniGrams(rawText)
    val bigrams = biGrams(rawText)
    val trigrams = triGrams(rawText)
    unigrams ++ bigrams ++ trigrams
  }

  /** Extract clean tokens without removing stop words
    */
  def cleanTokens(rawText: String): Seq[String] = {
    // Raw tokens without removing stop words
    // NOTE: here we DO NOT REMOVE stop words -- we use "rawTokenize"
    val rawTokens = tokenizer.rawTokenize(rawText.toLowerCase)
    // Drop tokens that are not alphabetical (“2”, “?”)
    rawTokens.filter(token => alphabetic.findFirstIn(token).nonEmpty)
  }

  /** Extract clean token with NO stop words
    */
  def contentTokens(rawText: String): Seq[String] = {
    // Lower-case the text before tokenizing
    // NOTE: here we use "keywordTokenize" to remove stopwords
    val basicTokens = tokenizer.keywordTokenize(rawText.toLowerCase)
    // Drop tokens that are not alphabetical (“2”, “?”)
    basicTokens.filter(token => alphabetic.findFirstIn(token).nonEmpty)
  }

  /** Extract unigram tokens from text.
    * No stop words; only content words.
    *
    * NOTE: This function may return an empty list. Be prepared!
    */
  def uniGrams(rawText: String): Seq[String] = {
    // Tokens without stopwords
    val tokens = contentTokens(rawText)
    // Drop tokens that are less than 3 characters long (“cm”)
    tokens.filter(_.length > 2)
  }

  /** Extract bigram tokens from text.
    * Bigrams are two consecutive non-stopwords. They cannot be
    * separated by stopwords in the raw text. Therefore we need
    * to identify the bigrams first and then remove the stopwords.
    */
  def biGrams(rawText: String): Seq[String] = {
    // Tokens with stopwords
    val tokens = cleanTokens(rawText).toIndexedSeq
    // Make a list of bigrams
    for {
      i <- 0 until (tokens.size - 1)
      // must have two content words
      if tokenizer.isKeyword(tokens(i)) && tokenizer.isKeyword(tokens(i + 1))
      bigram = tokens.slice(i, i + 2).mkString(" ")
      if bigram.length > 5 // drop bigrams that are less than 6 characters long ("to the")
    } yield bigram
  }

  /** Extract trigram tokens from text.
    * Trigrams can be three consecutive non-stopwords or two
    * non-stopwords with a stopword in between them.
    */
  def triGrams(rawText: String): Seq[String] = {
    // Tokens with stopwords
    val tokens = cleanTokens(rawText).toIndexedSeq
    // Make a list of trigrams
    for {
      i <- 0 until (tokens.size - 2)
      // must have two content words with a stop word or content word between them
      if tokenizer.isKeyword(tokens(i)) && tokenizer.isKeyword(tokens(i + 2))
      trigram = tokens.slice(i, i + 3).mkString(" ")
      if trigram.length > 8 // drop trigrams that are less than 9 characters long ("go to the")
    } yield trigram
  }

  /** Calculate PMI
    * Handling the special case of "A ... B" skip bigram patterns
    */
  def newPmi(question: String, answer: String): Future[Double] = {
    val lowVal: Double = 0
    if (ngramOverlap(question, answer)) {
      // If the question pattern overlaps with the answer, return a low value
      Future.successful(lowVal)
    } else {
      // If the input pattern is a pair sequence (skip bigram), then use special
      // queries for Wumpus and calculate the PMI
      val window = 10
      val corpusSize = 5e10.toLong
      // Transformations needed for Wumpus queries:
      // A B --> "A.B"
      // A B C --> "A.B.C"
      // A ... B --> "A".."B" -- skip bigram
      //
      // Question
      val qQuery = q(question).replace(" ... ", q("..")).replace(" ", ".")
      val qFreqFuture = if (qQuery.contains("..")) {
        wumpusCount(s" (($qQuery)<[$window])")
      } else {
        wumpusCount(s" ($qQuery)") // window not needed unless query uses ".."
      }
      val qProbFuture = qFreqFuture map { _.toDouble / corpusSize }
      //
      // Answer
      val aQuery = q(answer).replace(" ... ", q("..")).replace(" ", ".")
      val aFreqFuture = if (aQuery.contains("..")) {
        wumpusCount(s" (($aQuery)<[$window])")
      } else {
        wumpusCount(s" ($aQuery)") // window not needed unless query uses ".."
      }
      val aProbFuture = aFreqFuture map { _.toDouble / corpusSize }
      //
      // Question and Answer together
      val aqQuery = if (qQuery.contains("..") || aQuery.contains("..")) {
        s"($qQuery)^($aQuery)" // so that operator precedence is clear
      } else {
        s"$qQuery^$aQuery" // for backwards compatibility of query cache files
      }
      val aqFreqFuture = wumpusCount(s" (($aqQuery)<[$window])") // window always needed
      val aqProbFuture = aqFreqFuture map { _.toDouble / corpusSize }
      aqProbFuture flatMap { aqProb =>
        if (aqProb > 0) {
          aProbFuture.zip(qProbFuture) map {
            case (aProb, qProb) => Math.log(aqProb / (qProb * aProb))
          }
        } else {
          Future.successful(lowVal)
        }
      }
    }
  }

  /** Check two strings for overlap:
    * rabbit fur ~ fur
    * rabbit fur ~ rabbit
    * rabbit ~ rabbits
    */
  def ngramOverlap(string1: String, string2: String): Boolean = {
    string1.contains(string2) || string2.contains(string1)
  }

  /** Add quotes around a word, for building a Wumpus query
    */
  def q(word: String): String = {
    s""""$word""""
  }
}
