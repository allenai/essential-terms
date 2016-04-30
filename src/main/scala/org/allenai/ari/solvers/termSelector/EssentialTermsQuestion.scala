package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, Question }

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation

/** Various properties of an Aristo question relevant for essential term detection. */
case class EssentialTermsQuestion(
  rawQuestion: String,
  essentialTermMap: Option[Map[String, Double]],
  aristoQuestion: Question,
  questionTextAnnotation: TextAnnotation,
  salience: Option[List[(MultipleChoiceSelection, SalienceResult)]],
  sumSalience: Option[Map[String, Double]],
  maxSalience: Option[Map[String, Double]]
)
