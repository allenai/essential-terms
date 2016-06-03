package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, Question }

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, TextAnnotation }

import scala.collection.JavaConverters._

/** Various properties of an Aristo question relevant for essential term detection. */
case class EssentialTermsQuestion(
    rawQuestion: String,
    essentialTermMap: Option[Map[String, Double]],
    aristoQuestion: Question,
    questionTextAnnotation: TextAnnotation,
    salience: Option[List[(MultipleChoiceSelection, SalienceResult)]],
    sumSalience: Option[Map[String, Double]],
    maxSalience: Option[Map[String, Double]],
    numAnnotators: Option[Double]
) {
  /** Get non-stopword and stopword constituents for the question. */
  def getConstituents(stopWords: Set[String]): (Seq[Constituent], Seq[Constituent]) = {
    // whether to combine NER words together or not
    val cons = if (EssentialTermsSensors.combineNamedEntities) {
      EssentialTermsSensors.getCombinedConsituents(questionTextAnnotation)
    } else {
      questionTextAnnotation.getView(ViewNames.TOKENS).getConstituents.asScala.toSeq
    }
    cons.partition { c =>
      stopWords.contains(c.getSurfaceForm.toLowerCase())
    }
  }

  def getConstituents(constitunts: Seq[Constituent], stopWords: Set[String]): (Seq[Constituent], Seq[Constituent]) = {
    constitunts.partition { c => stopWords.contains(c.getSurfaceForm.toLowerCase()) }
  }
}
