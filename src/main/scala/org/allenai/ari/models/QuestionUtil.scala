package org.allenai.ari.models

import org.apache.commons.codec.digest.DigestUtils

/** Utility methods for questions. */
object QuestionUtil {
  val ShortHashLength = 8

  /** Returns a short hash of the given question string. */
  def shortHash(text: String): String = DigestUtils.sha1Hex(text).take(ShortHashLength)

  // pattern matching parenthesis
  private val PatternParens = """[()]+"""

  /** Compares two answer options and returns a boolean indicating if the two are the same after
    * removing characters which may exist in the answer option for formatting purposes (i.e.,
    * parenthesis).
    */
  def compareAnswers(a: String, b: String): Boolean = {
    a.replaceAll(PatternParens, "").toLowerCase == b.replaceAll(PatternParens, "").toLowerCase
  }
}
