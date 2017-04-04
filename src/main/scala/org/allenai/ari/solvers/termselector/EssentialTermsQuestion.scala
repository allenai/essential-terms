package org.allenai.ari.solvers.termselector

import org.allenai.ari.datamodel.salience.SalienceResult
import org.allenai.ari.datamodel.{ MultipleChoiceSelection, Question }

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation

/** Various properties of an Aristo question relevant for essential term detection.
  * @param rawQuestion the actual question
  * @param essentialTermMap a hashmap from string (a constituent in the question) to essentiality-score
  * @param aristoQuestion the actual question converted to [Question]
  * @param questionTextAnnotation different types of the annotations
  * @param salience salience score score for the given question
  * @param sumSalience aggregated salience scores, by summing the salience score across choices
  * @param maxSalience aggregated salience scores, by max-ing the salience score across choices
  * @param numAnnotators number of people who have annotated the question (only if it's already annotated)
  */
case class EssentialTermsQuestion(
  rawQuestion: String,
  essentialTermMap: Option[Map[String, Double]],
  aristoQuestion: Question,
  questionTextAnnotation: TextAnnotation,
  salience: Option[List[(MultipleChoiceSelection, SalienceResult)]],
  sumSalience: Option[Map[String, Double]],
  maxSalience: Option[Map[String, Double]],
  numAnnotators: Option[Double]
)
