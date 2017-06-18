package org.allenai.ari.models

import org.apache.commons.codec.digest.DigestUtils

/** The data associated with the direct-answer selection this corresponds to.
  * @param answer the answer from the question
  * @param index the zero-based integer index of this selection
  */
case class DirectAnswerSelection(answer: String, index: Int) {
  def toSelection = SolverSelection(None, Some(this))
}

object DirectAnswerSelection {
  import spray.json.DefaultJsonProtocol._
  implicit val shortAnswerSelectionJsonFormat = jsonFormat2(DirectAnswerSelection.apply)

  /** Computes and returns the reasoning hash for short-answer questions.
    */
  def reasoningHash(question: String, answer: String): String =
    DigestUtils.sha1Hex(s"$question: $answer")
}
