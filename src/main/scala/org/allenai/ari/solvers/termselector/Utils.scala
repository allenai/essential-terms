package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.{ MultipleChoiceSelection, SplitQuestion }
import org.allenai.common.Logging
import org.allenai.datastore.Datastore

import com.typesafe.config.Config

import scala.io.{ BufferedSource, Source }

import java.io.File

object Utils extends Logging {

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
  def getDatastoreFileAsSource(datastoreUriString: String): BufferedSource = {
    val (datastoreName, group, name, version) = parseDatastoreUri(datastoreUriString)
    getDatastoreFileAsSource(datastoreName, group, name, version)
  }

  /** Get a datastore file as a buffered Source. Caller is responsible for closing this stream. */
  def getDatastoreFileAsSource(config: Config): BufferedSource = {
    val datastoreName = config.getString("datastore")
    val group = config.getString("group")
    val name = config.getString("name")
    val version = config.getInt("version")
    getDatastoreFileAsSource(datastoreName, group, name, version)
  }

  /** Regex to use for parsing Datastore URIs in parseDatastoreUri() */
  private val datastoreUriRegex = """datastore://([^/]+)/([^/]+)/(.+)-v(\d+)(\..*)?""".r

  /** Parse datastore URIs such as the following to produce datastore name, group, file/folder name,
    * and version:
    * datastore://private/org.allenai.aristo.tables/Grade4-v10.json  (with extension)
    * datastore://private/org.allenai.aristo.tabledata/tables-v4  (without extension)
    */
  def parseDatastoreUri(datastoreUriString: String): (String, String, String, Int) = {
    datastoreUriString match {
      case datastoreUriRegex(datastoreName, group, basename, version, extension) =>
        val ext = if (extension == null) "" else extension // extension is optional
        (datastoreName, group, basename + ext, version.toInt)
      case _ => throw new IllegalArgumentException(s"Cannot parse $datastoreUriString")
    }
  }

  /** Get a datastore directory as a folder */
  def getDatastoreDirectoryAsFolder(
    datastoreName: String,
    group: String,
    name: String,
    version: Int
  ): File = {
    logger.debug(s"Loading directory from $datastoreName datastore: $group/$name-v$version")
    Datastore(datastoreName).directoryPath(group, name, version).toFile
  }

  /** Get a datastore directory as a folder */
  def getDatastoreDirectoryAsFolder(datastoreUriString: String): File = {
    val (datastoreName, group, name, version) = parseDatastoreUri(datastoreUriString)
    getDatastoreDirectoryAsFolder(datastoreName, group, name, version)
  }

  /** Get a datastore directory as a folder */
  def getDatastoreDirectoryAsFolder(config: Config): File = {
    val datastoreName = config.getString("datastore")
    val group = config.getString("group")
    val name = config.getString("name")
    val version = config.getInt("version")
    getDatastoreDirectoryAsFolder(datastoreName, group, name, version)
  }

  object Levenshtein {
    def minimum(i1: Int, i2: Int, i3: Int) = math.min(math.min(i1, i2), i3)
    def distance(s1: String, s2: String) = {
      val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) =>
        if (j == 0) i else if (i == 0) j else 0
      }
      for (j <- 1 to s2.length; i <- 1 to s1.length)
        dist(j)(i) = if (s2(j - 1) == s1(i - 1)) {
          dist(j - 1)(i - 1)
        } else {
          minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)
        }
      dist(s2.length)(s1.length)
    }

    def printDistance(s1: String, s2: String) =
      println("%s -> %s : %d".format(s1, s2, distance(s1, s2)))
  }
}
