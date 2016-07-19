package org.allenai.ari.solvers.termselector

import org.allenai.ari.models.salience.SalienceResult
import org.allenai.ari.models.{ MultipleChoiceSelection, Question }

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, TextAnnotation }

import scala.collection.JavaConverters._

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
) {
  /** Get non-stopword and stopword constituents for the question. */
  def splitConstituents(stopWords: Set[String]): (Seq[Constituent], Seq[Constituent]) = {
    // whether to combine NER words together or not
    val cons = if (Annotator.combineNamedEntities) {
      Annotator.getCombinedConsituents(questionTextAnnotation)
    } else {
      questionTextAnnotation.getView(ViewNames.TOKENS).getConstituents.asScala.toSeq
    }
    cons.partition { c => stopWords.contains(c.getSurfaceForm.toLowerCase()) }
  }

  /** Given a sequence of constituents, it splits them into two disjoint sequence, one contained in stopwords, one not.
    * @param constituents sequence of input constituents
    * @param stopWords sequence of stopwords
    * @return Pair of constituent sequences. First one doest not contain any stopwords, second one is all stopwords
    */
  def splitConstituents(constituents: Seq[Constituent], stopWords: Set[String]): (Seq[Constituent], Seq[Constituent]) = {
    constituents.partition { c => stopWords.contains(c.getSurfaceForm.toLowerCase()) }
  }
}
