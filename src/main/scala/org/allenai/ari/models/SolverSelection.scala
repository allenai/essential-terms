package org.allenai.ari.models

/** A container for mutually-exclusive multiple-choice and direct-answer selections.
  *
  * @param multipleChoice the multiple-choice selection
  * @param directAnswer the direct-answer selection
  */
case class SolverSelection(
    multipleChoice: Option[MultipleChoiceSelection],
    directAnswer: Option[DirectAnswerSelection]
) {
  require(
    multipleChoice.isDefined != directAnswer.isDefined,
    "Exactly one of `multipleChoice` and `directAnswer` must be defined."
  )

  // Aliases for brevity.
  private def mc = multipleChoice
  private def da = directAnswer

  // Helper accessors for sub-objects.
  def assertion = mc.map(_.assertion).getOrElse("")
  def answer = mc.map(_.answer).getOrElse(Some(da.get.answer))
  def focus = mc.map(_.focus).getOrElse("")
  def key = mc.map(_.key).getOrElse("")
  def index = mc.map(_.index).getOrElse(da.get.index)

  def isMultipleChoice = mc.isDefined
}

object SolverSelection {
  import spray.json.DefaultJsonProtocol._
  implicit val shortAnswerSelectionJsonFormat = jsonFormat2(SolverSelection.apply)
}
