package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

import scala.collection.JavaConverters._

/** TODO: add description */
class BaselineDataModel extends IllinoisDataModel {
  override val essentialTermTokens = node[Constituent]

  override val goldLabel = property(essentialTermTokens) { x: Constituent =>
    x.getTextAnnotation
      .getView(EssentialTermsConstants.VIEW_NAME)
      .getConstituentsCovering(x)
      .get(0)
      .getLabel
  }

  val wordForm = property(essentialTermTokens, cache = true) { x: Constituent =>
    x.toString
  }

  val pos = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.POS).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val lemma = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.LEMMA).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val ner = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.NER_CONLL).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val posConjLemma = property(essentialTermTokens) { x: Constituent =>
    pos(x) + lemma(x)
  }

  val posConjWordform = property(essentialTermTokens) { x: Constituent =>
    pos(x) + wordForm(x)
  }

  val posConjNer = property(essentialTermTokens) { x: Constituent =>
    pos(x) + ner(x)
  }

  val lemmaConjNer = property(essentialTermTokens) { x: Constituent =>
    lemma(x) + ner(x)
  }

  val wordFormConjNer = property(essentialTermTokens) { x: Constituent =>
    wordForm(x) + ner(x)
  }

  val wordFormConjNerConjPOS = property(essentialTermTokens) { x: Constituent =>
    wordForm(x) + ner(x) + pos(x)
  }
}
