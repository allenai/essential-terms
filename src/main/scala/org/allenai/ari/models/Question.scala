package org.allenai.ari.models

/** A structured question.
  * @param rawQuestion the question literal from the input source, including answers
  * @param text the question (non-answer) portion of the raw question. Should not be None, is
  * declared as Option[String] for legacy reasons.
  * @param selections the structured answer options built from the original question. There will be
  * one of these per answer option available.
  */
case class Question(
    rawQuestion: String,
    text: Option[String],
    selections: Seq[MultipleChoiceSelection]
) {
  @transient lazy val shortHash: String = QuestionUtil.shortHash(rawQuestion)
  def isMultipleChoice: Boolean = selections.size > 1
}

object Question {
  import spray.json.DefaultJsonProtocol._
  implicit val multipleChoiceQuestionJsonFormat = jsonFormat3(Question.apply)
}
