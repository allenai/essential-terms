package org.allenai.ari.models

import scala.annotation.tailrec

/** A ChoiceIdentifier takes a "question passage" as a raw string and returns a SplitQuestion. */
trait ChoiceIdentifier extends (String => SplitQuestion)

object ChoiceIdentifier {
  // Currently we support multiple choice questions with up to 10 choices.
  val AllChoiceKeys = List(
    List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
    List("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"),
    (0 to 10) map { _.toString }
  )

  /** Takes two string lists and interleaves them.
    *
    * For instance, interleave(List("a", "b", "c"), List("d", "e", "f")) should return
    * List("a", "d", "b", "e", "c", "f").
    *
    * If the two lists are not the same length, the shorter list is padded with empty strings.
    *
    * @param list1 the first string list
    * @param list2 the second string list
    * @return the interleaved list
    */
  def interleave(list1: Seq[String], list2: Seq[String]): Seq[String] = {
    (list1.zipAll(list2, "", "") flatMap { case (el1, el2) => List(el1, el2) })
  }

  /** Runs the classical dynamic programming solution to Longest Common Subsequence.
    *
    * For instance, if sequence a is "abcdefghijkl" and sequence b is "bdafgm", then the
    * return value would be (Seq(1,3,5,6), Seq(0,1,3,4)), i.e. the indices of the longest
    * common subsequence bdfg in sequences a and b.
    *
    * Note that the longest common subsequence may not be unique.
    *
    * This code is adapted from http://rosettacode.org/wiki/Longest_common_subsequence.
    *
    * @param a the first string list
    * @param b the second string list
    * @return a pair of integer sequences that give the indices of the LCS in sequences a and b
    */
  def longestCommonSubsequence(a: Seq[String], b: Seq[String]): (Seq[Int], Seq[Int]) = {
    if (a.size == 0 || b.size == 0) {
      (Seq(), Seq())
    } else if (a == b) {
      (Range(0, a.size), Range(0, b.size))
    } else {
      val lengths = Array.ofDim[Int](a.size + 1, b.size + 1)
      for (i <- 0 until a.size) {
        for (j <- 0 until b.size) {
          if (a(i) == b(j)) {
            lengths(i + 1)(j + 1) = lengths(i)(j) + 1
          } else {
            lengths(i + 1)(j + 1) = scala.math.max(lengths(i + 1)(j), lengths(i)(j + 1))
          }
        }
      }
      // read the substring out from the matrix
      var commonIndicesA: Seq[Int] = Seq()
      var commonIndicesB: Seq[Int] = Seq()
      var x = a.size
      var y = b.size
      do {
        if (lengths(x)(y) == lengths(x - 1)(y)) {
          x -= 1
        } else if (lengths(x)(y) == lengths(x)(y - 1)) {
          y -= 1
        } else {
          assert(a(x - 1) == b(y - 1))
          commonIndicesA = (x - 1) +: commonIndicesA
          commonIndicesB = (y - 1) +: commonIndicesB
          x -= 1
          y -= 1
        }
      } while (x != 0 && y != 0)
      (commonIndicesA, commonIndicesB)
    }
  }
}

/** A BracketedChoiceIdentifier identifies any multiple choice format with bracketed answer
  * keys, e.g.:
  *
  * What is an apple? (a) a fruit (b) a veggie
  * What is an apple? [i] a fruit [ii] a veggie
  *
  * The "choice map" returned by the apply operator (see ChoiceIdentifier) will map the
  * choice keys (with bracketing stripped) to the whitespace-trimmed choice associated with
  * each key. See the unit tests in ChoiceIdentifierSpec for specific examples and edge cases.
  *
  * The identification procedure finds the longest sequence of choice keys that is a
  * subsequence of one of the "choice key" lists in ChoiceIdentifier.AllChoiceKeys. It treats
  * any other bracketed item as ordinary text. This allows questions to contain parentheticals
  * as in:
  *
  * What is a (Granny Smith) apple? (a) a fruit (b) a veggie
  *
  * Here, (Granny Smith) will not be treated as a choice key.
  *
  * This is an abstract class. You need to override the .findBrackets method for each bracketing
  * style (e.g. parentheticals or square brackets).
  */
abstract class BracketedChoiceIdentifier extends ChoiceIdentifier {

  override def apply(passageStr: String): SplitQuestion = {
    val (prologue, bracketings, followons) = findBrackets(passageStr)
    val strippedBracketings = bracketings map { x => x.slice(1, x.size - 1) }

    // Find the subsequence of parentheticals that looks the most like an enumeration.
    val allValidBracketings: Seq[(Int, Seq[Int])] = {
      for (answerIndices: Seq[String] <- ChoiceIdentifier.AllChoiceKeys) yield {
        val (_, subseqB) = ChoiceIdentifier.longestCommonSubsequence(
          answerIndices map { _.toLowerCase },
          strippedBracketings map { _.toLowerCase }
        )
        (subseqB.size, subseqB)
      }
    }
    val bestBracketings = ((allValidBracketings.maxBy { case (length, _) => length })._2
      :+ bracketings.size).toSeq // append bracketings.size for edge case

    // Reconstruct the choice text for the "best" bracketings (i.e. put the incorrectly
    // segmented bracketings back into their respective choice text).
    val reconstructedChoiceText: Seq[String] = {
      bestBracketings.dropRight(1).zipWithIndex map {
        case (followonIndex, index) => {
          if (followonIndex + 1 < bestBracketings(index + 1)) {
            ChoiceIdentifier.interleave(
              followons.slice(followonIndex, bestBracketings(index + 1)),
              bracketings.slice(followonIndex + 1, bestBracketings(index + 1))
            ) reduce { (x, y) => x + y }
          } else {
            followons.slice(followonIndex, bestBracketings(index + 1)) reduce { (x, y) => x + y }
          }
        }
      } map { _.trim }
    }

    // Reconstruct the question text (which may contain bracketings that aren't part of the "best
    // bracketing" and thus been incorrectly segmented).
    val reconstruction: String = bestBracketings.headOption match {
      case Some(x) if x >= 1 => {
        (ChoiceIdentifier.interleave(
          bracketings.take(x),
          followons.take(x)
        ) reduce { (x, y) => x + y })
      }
      case _ => ""
    }
    val question = (prologue + reconstruction).trim
    val goodStrippedBracketings = bestBracketings.dropRight(1) map {
      strippedBracketings
    }
    val keyAnswerPairs = goodStrippedBracketings zip reconstructedChoiceText
    SplitQuestion(question, keyAnswerPairs)
  }

  /** In a raw string, finds all simple bracketed text (i.e. with no nested bracketing).
    *
    * For instance, for string "What is an apple? (a) a fruit (b) a veggie" it should return the
    * triple:
    *
    * ("What is an apple?", Seq("(a)", "(b)"), Seq(" a fruit ", " a veggie"))
    *
    * The first element is the string before the first bracketing. The second and third elements
    * of the triple are equivalent-length lists that respectively contain the bracket contents
    * (bracketing preserved) and the text following each bracketing (whitespace preserved).
    *
    * @param inputStr the raw string to process
    * @return a triple (text, bracketContents, followonText), described above
    */
  def findBrackets(inputStr: String): (String, Seq[String], Seq[String])
}

/** A ChoiceIdentifier designed for square brackets. */
object SquareBracketedChoiceIdentifier extends BracketedChoiceIdentifier {

  val bracketPattern = """(.*)(\[[^\[\]]+\])(.*)""".r

  override def findBrackets(inputStr: String): (String, Seq[String], Seq[String]) = {
    findSquareBrackets(inputStr)
  }

  @tailrec private def findSquareBrackets(
                                           inputStr: String,
                                           parentheticalsSoFar: Seq[String] = Seq(),
                                           followonSoFar: Seq[String] = Seq()
                                         ): (String, Seq[String], Seq[String]) = {

    inputStr match {
      case bracketPattern(unprocessed, parenthetical, followon) =>
        findSquareBrackets(unprocessed, parenthetical +: parentheticalsSoFar,
          followon +: followonSoFar)
      case _ => (inputStr, parentheticalsSoFar, followonSoFar)
    }
  }
}

/** A ChoiceIdentifier designed for parentheticals. */
object ParentheticalChoiceIdentifier extends BracketedChoiceIdentifier {

  val parenPattern = """(.*)(\([^\(\)]+\))(.*)""".r

  override def findBrackets(inputStr: String): (String, Seq[String], Seq[String]) = {
    findParentheticals(inputStr)
  }

  @tailrec private def findParentheticals(
                                           inputStr: String,
                                           parentheticalsSoFar: Seq[String] = Seq(),
                                           followonSoFar: Seq[String] = Seq()
                                         ): (String, Seq[String], Seq[String]) = {

    inputStr match {
      case parenPattern(unprocessed, parenthetical, followon) =>
        findParentheticals(unprocessed, parenthetical +: parentheticalsSoFar,
          followon +: followonSoFar)
      case _ => (inputStr, parentheticalsSoFar, followonSoFar)
    }
  }
}
