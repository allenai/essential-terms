package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.{ MultipleChoiceSelection, SplitQuestion }
import org.allenai.common.Logging
import org.allenai.datastore.Datastore

import com.typesafe.config.Config

import scala.io.{ BufferedSource, Source }

object EssentialTermsUtils extends Logging {

  // copied from Controller.scala, the method is private
  def fallbackDecomposer(splitQuestion: SplitQuestion): Seq[MultipleChoiceSelection] = {
    for {
      ((key, answer), i) <- splitQuestion.keyAnswerPairs.zipWithIndex
      question = splitQuestion.question.stripSuffix("?").stripSuffix(".").stripSuffix("!")
      assertion = s"Is it true that $question $answer?"
    } yield MultipleChoiceSelection(assertion, Some(answer), answer, key, i)
  }

  /** Get a datastore file as a buffered Source. Caller is responsible for closing this stream. */
  def getDatastoreFileAsSource(
    datastoreName: String,
    group: String,
    name: String,
    version: Int
  ): BufferedSource = {
    logger.debug(s"Loading file from $datastoreName datastore: $group/$name-v$version")
    val file = Datastore(datastoreName).filePath(group, name, version).toFile
    Source.fromFile(file)
  }

  /** Get a datastore file as a buffered Source. Caller is responsible for closing this stream. */
  def getDatastoreFileAsSource(config: Config): BufferedSource = {
    val datastoreName = config.getString("datastore")
    val group = config.getString("group")
    val name = config.getString("name")
    val version = config.getInt("version")
    getDatastoreFileAsSource(datastoreName, group, name, version)
  }

  object Levenshtein {
    def minimum(i1: Int, i2: Int, i3: Int) = math.min(math.min(i1, i2), i3)
    def distance(s1: String, s2: String) = {
      val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }
      for (j <- 1 to s2.length; i <- 1 to s1.length)
        dist(j)(i) = if (s2(j - 1) == s1(i - 1)) {
          dist(j - 1)(i - 1)
        } else {
          minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)
        }
      dist(s2.length)(s1.length)
    }

    def printDistance(s1: String, s2: String) = println("%s -> %s : %d".format(s1, s2, distance(s1, s2)))
  }
}
