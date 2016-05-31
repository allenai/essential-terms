package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.edison.features.factory.WordFeatureExtractorFactory

import scala.collection.JavaConverters._

/** This class contains the basic definitions and properties of the model */
class BaselineDataModel extends IllinoisDataModel {
  override val essentialTermTokens = node[Constituent]

  override val goldLabel = property(essentialTermTokens) { x: Constituent =>
    x.getTextAnnotation
      .getView(EssentialTermsConstants.VIEW_NAME)
      .getConstituentsCovering(x)
      .get(0)
      .getLabel
  }

  // the gold label of two consecutive constituents
  val goldLabelPair = property(essentialTermTokens) { x: Constituent =>
    val xBefore = EssentialTermsSensors.getConstituentBefore(x, viewName = ViewNames.TOKENS)
    goldLabel(xBefore) + EssentialTermsConstants.LABEL_SEPARATOR + goldLabel(x)
  }

  val wordForm = property(essentialTermTokens) { x: Constituent =>
    x.toString
  }

  val wordFormPair = property(essentialTermTokens) { x: Constituent =>
    val xBefore = EssentialTermsSensors.getConstituentBefore(x, viewName = ViewNames.TOKENS)
    wordForm(xBefore) + EssentialTermsConstants.LABEL_SEPARATOR + wordForm(x)
  }

  val pos = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.POS).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  // The coarse POS tag (one of Noun, Verb, Adjective, Adverb, Punctuation, Pronoun and Other)
  val conflatedPos = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.conflatedPOS.getFeatures(x).asScala.mkString
  }

  val lemma = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.LEMMA).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val lemmaPair = property(essentialTermTokens) { x: Constituent =>
    val xBefore = EssentialTermsSensors.getConstituentBefore(x, viewName = ViewNames.TOKENS)
    lemma(xBefore) + EssentialTermsConstants.LABEL_SEPARATOR + lemma(x)
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

  val conflatedPosConjLemma = property(essentialTermTokens) { x: Constituent =>
    conflatedPos(x) + lemma(x)
  }

  val conflatedPosConjWordform = property(essentialTermTokens) { x: Constituent =>
    conflatedPos(x) + wordForm(x)
  }

  val posConjNer = property(essentialTermTokens) { x: Constituent =>
    pos(x) + ner(x)
  }

  val conflatedPosConjNer = property(essentialTermTokens) { x: Constituent =>
    conflatedPos(x) + ner(x)
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
