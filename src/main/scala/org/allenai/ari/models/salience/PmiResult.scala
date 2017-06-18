package org.allenai.ari.models.salience

/** A PMI score between a set of question string(s) and answer string(s). */
case class PmiResult(questionPattern: String, answerPattern: String, value: Double)

object PmiResult {
  import spray.json.DefaultJsonProtocol._
  implicit val pmiResultFormat = jsonFormat3(PmiResult.apply)
}
