package org.allenai.ari.models

/** The data associated with the multiple-choice selection this corresponds to.
  * @param assertion the text of the true / false assertion in English, e.g. "Is it true that blue
  * is a color?"
  * @param answer the answer from the question. Will be missing for older serializations, or if
  * question splitting fails.
  * @param focus the answer, possibly rephrased for the assertion. Part of the assertion string
  * should match this text exactly.
  * @param key the answer's label in the original question (i.e. "(A)" or "2")
  * @param index the zero-based integer index of the assertion in the answer options
  */
case class MultipleChoiceSelection(
    assertion: String,
    answer: Option[String],
    focus: String,
    key: String,
    index: Int
) {
  def toSelection = SolverSelection(Some(this), None)
}
object MultipleChoiceSelection {
  import spray.json.DefaultJsonProtocol._
  implicit val multipleChoiceSelectionJsonFormat = jsonFormat5(MultipleChoiceSelection.apply)
}
