package org.allenai.ari.models.salience

/** A result returned from the salience scorer.
  * @param scores a mapping of token to salience score
  * @param pmiResults all of the PMI results generated when calculating the salience scores
  */
case class SalienceResult(scores: Map[String, Double], pmiResults: Seq[PmiResult])

object SalienceResult {
  import spray.json.DefaultJsonProtocol._
  implicit val salienceResultFormat = jsonFormat2(SalienceResult.apply)
}
