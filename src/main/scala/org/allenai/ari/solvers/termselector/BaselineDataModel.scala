package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

import scala.collection.JavaConverters._

/** TODO: add description */
class BaselineDataModel extends IllinoisDataModel {
  override val tokens = node[Constituent]

  override val goldLabel = property(tokens) { x: Constituent =>
    x.getTextAnnotation
      .getView(EssentialTermsConstants.VIEW_NAME)
      .getConstituentsCovering(x)
      .get(0)
      .getLabel
  }

  val wordForm = property(tokens, "wordForm", cache = true) { x: Constituent =>
    x.toString
  }

  val pos = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.POS).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val lemma = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.LEMMA).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val ner = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.NER_CONLL).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }
}
