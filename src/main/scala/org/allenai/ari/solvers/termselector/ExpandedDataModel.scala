package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.edison.features.factory.BrownClusterFeatureExtractor
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property
import org.allenai.ari.solvers.termselector.EssentialTermsSensors._

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

import scala.collection.JavaConverters._

/** TODO(daniel) add description */
class ExpandedDataModel(
    baselineDataModel: BaselineDataModel,
    baselineClassifiers: BaselineLearners
) extends IllinoisDataModel {

  // first copy relevant fields from BaselineDataModel and the underlying (mutable) DataModel
  NODES ++= baselineDataModel.NODES
  EDGES ++= baselineDataModel.EDGES
  PROPERTIES ++= baselineDataModel.PROPERTIES
  //propertyCacheList ++= baselineDataModel.propertyCacheList
  override val essentialTermTokens = baselineDataModel.essentialTermTokens
  //  override val allTokens = baselineDataModel.allTokens
  override val goldLabel = baselineDataModel.goldLabel
  val wordForm = baselineDataModel.wordForm
  val lemma = baselineDataModel.lemma
  val ner = baselineDataModel.ner
  val pos = baselineDataModel.pos
  val posConjLemma = baselineDataModel.posConjLemma
  val posConjWordform = baselineDataModel.posConjWordform
  val posConjNer = baselineDataModel.posConjNer
  val lemmaConjNer = baselineDataModel.lemmaConjNer
  val wordFormConjNer = baselineDataModel.wordFormConjNer
  val wordFormConjNerConjPOS = baselineDataModel.wordFormConjNerConjPOS

  // now populate additional fields
  //  val constituentAfter = edge(essentialTermTokens, allTokens)
  //  constituentAfter.addSensor(getConstituentAfter _)
  //
  //  val constituentBefore = edge(essentialTermTokens, allTokens)
  //  constituentBefore.addSensor(getConstituentBefore _)
  //
  //  val constituentTwoAfter = edge(essentialTermTokens, allTokens)
  //  constituentTwoAfter.addSensor(getConstituentTwoAfter _)
  //
  //  val constituentTwoBefore = edge(essentialTermTokens, allTokens)
  //  constituentTwoBefore.addSensor(getConstituentTwoBefore _)

  val baselineTargetP = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    b(x)
  }

  val labelOrBaseline = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    if (b.isTraining) {
      goldLabel(x)
    } else if (b.classifier.observed(wordForm(x))) {
      b(x)
    } else {
      ""
    }
  }

  val labelOneBefore = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  val labelTwoBefore = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoBefore(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  val labelOneAfter = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentAfter(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  val labelTwoAfter = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoAfter(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  // label 2-before conjunction with label 1-before
  val L2bL1b = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    labelTwoBefore(b)(x) + "-" + labelOneBefore(b)(x)
  }

  // label 1-before conjunction with label 1-after
  val L1bL1a = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    labelOneBefore(b)(x) + "-" + labelOneAfter(b)(x)
  }

  // label 1-after conjunction with label 2-after
  val L1aL2a = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    labelOneAfter(b)(x) + "-" + labelTwoAfter(b)(x)
  }

  private def baselineProperties(b: BaselineLearner): List[Property[Constituent]] = {
    List(
      baselineTargetP(b),
      labelTwoBefore(b),
      labelOneBefore(b),
      labelOneAfter(b),
      labelTwoAfter(b),
      L2bL1b(b),
      L1bL1a(b),
      L1aL2a(b)
    )
  }

  val baselinePropertiesSurfaceForm = baselineProperties(baselineClassifiers.surfaceForm)

  val baselinePropertiesLemma = baselineProperties(baselineClassifiers.lemma)

  val baselinePropertiesPOSConjLemma = baselineProperties(baselineClassifiers.posConjLemma)

  val baselinePropertiesPOSConjNer = baselineProperties(baselineClassifiers.wordFormConjNer)

  val baselinePropertiesPOSConjNerConjPos = baselineProperties(baselineClassifiers.wordFormConjNerConjPos)

  val isAScienceTerm = property(essentialTermTokens) { x: Constituent =>
    scienceTerms.contains(wordForm(x))
  }

  val isAScienceTermConjPos = property(essentialTermTokens) { x: Constituent =>
    scienceTerms.contains(wordForm(x)) + pos(x)
  }

  val isAScienceTermLemma = property(essentialTermTokens) { x: Constituent =>
    scienceTerms.contains(lemma(x))
  }

  val srlLabel = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.SRL_VERB).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val isItCapitalized = property(essentialTermTokens) { x: Constituent =>
    val word = wordForm(x)
    val firstCharacter: String = word.substring(0, 1)
    val upperCase: String = firstCharacter.toUpperCase
    upperCase.matches("[A-Z]") && (upperCase == firstCharacter)
  }

  val whKeyWords = Set("which", "what", "where", "when", "how")

  val afterWHword = property(essentialTermTokens) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    whKeyWords.contains(wordForm(cons).toLowerCase)
  }

  val afterWHwordWorForm = property(essentialTermTokens) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    if (whKeyWords.contains(wordForm(cons).toLowerCase)) wordForm(x) else ""
  }

  val twoAfterWHword = property(essentialTermTokens) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoBefore(y.head)
    whKeyWords.contains(wordForm(cons).toLowerCase)
  }

  // returns the word after the given word in the given view
  val wordAfter = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentAfter(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  // checks whether the wordAfter in the given view is the same as the word given in the input.
  val afterWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordAfter(view)(x).toString == word
    }
  }

  val wordTwoAfter = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentTwoAfter(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  val twoAfterWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordTwoAfter(view)(x).toString == word
    }
  }

  val wordBefore = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentBefore(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  val beforeWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordBefore(view)(x).toString == word
    }
  }

  val wordTwoBefore = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentTwoBefore(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  val twoBeforeWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordTwoBefore(view)(x).toString == word
    }
  }

  val isItLastSentence = property(essentialTermTokens) { x: Constituent =>
    x.getSentenceId == (x.getTextAnnotation.getNumberOfSentences - 1)
  }

  val isItSecondToLastSentence = property(essentialTermTokens) { x: Constituent =>
    x.getSentenceId == (x.getTextAnnotation.getNumberOfSentences - 2)
  }

  val isItCloseToEnd = property(essentialTermTokens) { x: Constituent =>
    x.getSentenceId / x.getTextAnnotation.getNumberOfSentences
  }

  val w2v = property(essentialTermTokens, cache = true) { x: Constituent =>
    val key = if (w2vModel.forSearch().contains(wordForm(x))) wordForm(x) else w2vNoMatchStr
    w2vModel.forSearch().getRawVector(key).asScala.map(_.doubleValue()).toList.head
  }

  val maxSalience = property(essentialTermTokens) { x: Constituent =>
    val salienceOpt = constituentToAnnotationMap(x).maxSalience
    salienceOpt match {
      case Some(s) => s.getOrElse(wordForm(x), 0d)
      case None => 0d
    }
  }

  val sumSalience = property(essentialTermTokens) { x: Constituent =>
    val salienceOpt = constituentToAnnotationMap(x).sumSalience
    salienceOpt match {
      case Some(s) => s.getOrElse(wordForm(x), 0d)
      case None => 0d
    }
  }

  val chunkLabel = property(essentialTermTokens) { x: Constituent =>
    val chunkView = constituentToAnnotationMap(x).questionTextAnnotation.getView(ViewNames.SHALLOW_PARSE)
    chunkView.getLabelsCovering(x).asScala.toList
  }

  //  val brownClusterFeatures = property(essentialTermTokens) { x: Constituent =>
  //    BrownClusterFeatureExtractor.getWordFeatures()
  //  }

}
