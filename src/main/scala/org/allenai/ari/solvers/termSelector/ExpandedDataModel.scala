package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.EssentialTermsSensors._

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

import scala.collection.JavaConverters._

/** TODO(daniel) add description */
class ExpandedDataModel(
    baselineDataModel: BaselineDataModel,
    baselineClassifier: BaselineLearner
) extends EssentialTermsDataModel {

  // first copy relevant fields from BaselineDataModel and the underlying (mutable) DataModel
  NODES ++= baselineDataModel.NODES
  EDGES ++= baselineDataModel.EDGES
  PROPERTIES ++= baselineDataModel.PROPERTIES
  propertyCacheList ++= baselineDataModel.propertyCacheList
  override val tokens = baselineDataModel.tokens
  override val goldLabel = baselineDataModel.goldLabel
  val wordForm = baselineDataModel.wordForm

  // now populate additional fields
  val constituentAfter = edge(tokens, tokens)
  constituentAfter.addSensor(getConstituentAfter _)

  val constituentBefore = edge(tokens, tokens)
  constituentBefore.addSensor(getConstituentBefore _)

  val constituentTwoAfter = edge(tokens, tokens)
  constituentTwoAfter.addSensor(getConstituentTwoAfter _)

  val constituentTwoBefore = edge(tokens, tokens)
  constituentTwoBefore.addSensor(getConstituentTwoBefore _)

  val baselineTarget = property(tokens, "baselineTarget", cache = true) { x: Constituent =>
    baselineClassifier(x)
  }

  val labelOrBaseline = property(tokens, "labelOrBaseline", cache = true) { x: Constituent =>
    if (baselineClassifier.isTraining) {
      goldLabel(x)
    } else if (baselineClassifier.classifier.observed(wordForm(x))) {
      baselineClassifier.classifier.discreteValue(x)
    } else {
      ""
    }
  }

  val labelOneBefore = property(tokens, "labelOneBefore", cache = true) { x: Constituent =>
    val cons = (tokens(x) ~> constituentBefore).head
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (baselineClassifier.isTraining) goldLabel(cons) else baselineClassifier(cons)
    } else {
      ""
    }
  }

  val labelTwoBefore = property(tokens, "labelTwoBefore", cache = true) { x: Constituent =>
    val cons = (tokens(x) ~> constituentTwoBefore).head
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (baselineClassifier.isTraining) goldLabel(cons) else baselineClassifier(cons)
    } else {
      ""
    }
  }

  val labelOneAfter = property(tokens, "labelOneAfter", cache = true) {
    x: Constituent =>
      val cons = (tokens(x) ~> constituentAfter).head
      // make sure the spans are different. Otherwise it is not valid
      if (cons.getSpan != x.getSpan) labelOrBaseline(cons) else ""
  }

  val labelTwoAfter = property(tokens, "labelTwoAfter", cache = true) { x: Constituent =>
    val cons = (tokens(x) ~> constituentTwoAfter).head
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(cons) else ""
  }

  val L2bL1b = property(tokens, "label2beforeLabel1beforeConjunction") { x: Constituent =>
    labelTwoBefore(x) + "-" + labelOneBefore(x)
  }

  val L1bL1a = property(tokens, "label1beforeLabel1afterConjunction") { x: Constituent =>
    labelOneBefore(x) + "-" + labelOneAfter(x)
  }

  val L1aL2a = property(tokens, "labelfterLabel2AfterConjunction") { x: Constituent =>
    labelOneAfter(x) + "-" + labelTwoAfter(x)
  }

  val isAScienceTerm = property(tokens) { x: Constituent =>
    //if (scienceTerms.contains(wordForm(x)))
    //  println(s"${wordForm(x)} is science term! ")
    scienceTerms.contains(wordForm(x))
  }

  val posLabel = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.POS).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val lemma = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.LEMMA).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val nerLabel = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.NER_CONLL).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val srlLabel = property(tokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.SRL_VERB).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  val isItCapitalized = property(tokens) { x: Constituent =>
    val word = wordForm(x)
    val firstCharacter: String = word.substring(0, 1)
    val upperCase: String = firstCharacter.toUpperCase
    upperCase.matches("[A-Z]") && (upperCase == firstCharacter)
  }

  val salienceScore = property(tokens) { x: Constituent =>
    1 // TODO
  }

  val pmiScore = property(tokens) { x: Constituent =>
    1 // TODO
  }

  val whKeyWords = Set("which", "what", "where", "when", "how")

  val afterWHword = property(tokens) { x: Constituent =>
    val cons = (tokens(x) ~> constituentBefore).head
    whKeyWords.contains(wordForm(cons).toLowerCase)
  }

  val twoAfterWHword = property(tokens) { x: Constituent =>
    val cons = (tokens(x) ~> constituentTwoBefore).head
    whKeyWords.contains(wordForm(cons).toLowerCase)
  }

  val afterWord = { (word: String, view: String) =>
    property(tokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val cons = EssentialTermsSensors.getConstituentAfter(y.head)
      word == wordForm(cons).toLowerCase
    }
  }

  val twoAfterWord = { (word: String, view: String) =>
    property(tokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val cons = EssentialTermsSensors.getConstituentTwoAfter(y.head)
      word == wordForm(cons).toLowerCase
    }
  }

  val beforeWord = { (word: String, view: String) =>
    property(tokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val cons = EssentialTermsSensors.getConstituentBefore(y.head)
      word == wordForm(cons).toLowerCase
    }
  }

  val twoBeforeWord = { (word: String, view: String) =>
    property(tokens) { x: Constituent =>
      val y = getConstituentCoveringInView(x, view).asScala
      val cons = EssentialTermsSensors.getConstituentTwoBefore(y.head)
      word == wordForm(cons).toLowerCase
    }
  }

  val isItLastSentence = property(tokens) { x: Constituent =>
    x.getSentenceId == (x.getTextAnnotation.getNumberOfSentences - 1)
  }

  val isItCloseToEnd = property(tokens) { x: Constituent =>
    x.getSentenceId / x.getTextAnnotation.getNumberOfSentences
  }

  val isItCloseToBeginning = property(tokens) { x: Constituent =>
    1 - x.getSentenceId / x.getTextAnnotation.getNumberOfSentences
  }

  val w2v = property(tokens, cache = true) { x: Constituent =>
    val key = if (w2vModel.forSearch().contains(wordForm(x))) wordForm(x) else w2vNoMatchStr
    w2vModel.forSearch().getRawVector(key).asScala.map(_.doubleValue()).toList.head
  }

  val maxSalience = property(tokens) { x: Constituent =>
    val salienceOpt = constituentToAnnotationMap(x).maxSalience
    salienceOpt match {
      case Some(s) => s.getOrElse(wordForm(x), 0d)
      case None => 0d
    }
  }

  val sumSalience = property(tokens) { x: Constituent =>
    val salienceOpt = constituentToAnnotationMap(x).sumSalience
    salienceOpt match {
      case Some(s) => s.getOrElse(wordForm(x), 0d)
      case None => 0d
    }
  }
}
