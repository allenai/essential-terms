package org.allenai.ari.solvers.termselector

import org.allenai.ari.solvers.termselector.EssentialTermsSensors._

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

import scala.collection.JavaConverters._

/** TODO(daniel) add description */
class ExpandedDataModel(
    baselineDataModel: BaselineDataModel,
    val baselineClassifiers: BaselineLearners
) extends IllinoisDataModel {

  // first copy relevant fields from BaselineDataModel and the underlying (mutable) DataModel
  NODES ++= baselineDataModel.NODES
  EDGES ++= baselineDataModel.EDGES
  PROPERTIES ++= baselineDataModel.PROPERTIES
  //propertyCacheList ++= baselineDataModel.propertyCacheList
  override val tokens = baselineDataModel.tokens
  override val goldLabel = baselineDataModel.goldLabel
  val wordForm = baselineDataModel.wordForm
  val lemma = baselineDataModel.lemma
  val ner = baselineDataModel.ner
  val pos = baselineDataModel.pos

  // now populate additional fields
  val constituentAfter = edge(tokens, tokens)
  constituentAfter.addSensor(getConstituentAfter _)

  val constituentBefore = edge(tokens, tokens)
  constituentBefore.addSensor(getConstituentBefore _)

  val constituentTwoAfter = edge(tokens, tokens)
  constituentTwoAfter.addSensor(getConstituentTwoAfter _)

  val constituentTwoBefore = edge(tokens, tokens)
  constituentTwoBefore.addSensor(getConstituentTwoBefore _)

  val baselineTarget = (b: BaselineLearner) => property(tokens, cache = true) { x: Constituent => b(x) }

  val labelOrBaseline = (b: BaselineLearner) => property(tokens, cache = true) { x: Constituent =>
    if (b.isTraining) {
      goldLabel(x)
    } else if (b.classifier.observed(wordForm(x))) {
      b(x)
    } else {
      ""
    }
  }

  val labelOneBefore = (b: BaselineLearner) => property(tokens, cache = true) { x: Constituent =>
    val cons = (tokens(x) ~> constituentBefore).head
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  val labelTwoBefore = (b: BaselineLearner) => property(tokens, cache = true) { x: Constituent =>
    val cons = (tokens(x) ~> constituentTwoBefore).head
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  val labelOneAfter = (b: BaselineLearner) => property(tokens, cache = true) {
    x: Constituent =>
      val cons = (tokens(x) ~> constituentAfter).head
      // make sure the spans are different. Otherwise it is not valid
      if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  val labelTwoAfter = (b: BaselineLearner) => property(tokens, cache = true) { x: Constituent =>
    val cons = (tokens(x) ~> constituentTwoAfter).head
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  // label 2-before conjunction with label 1-before
  val L2bL1b = (b: BaselineLearner) => property(tokens) { x: Constituent =>
    labelTwoBefore(b)(x) + "-" + labelOneBefore(b)(x)
  }

  // label 1-before conjunction with label 1-after
  val L1bL1a = (b: BaselineLearner) => property(tokens) { x: Constituent =>
    labelOneBefore(b)(x) + "-" + labelOneAfter(b)(x)
  }

  // label 1-after conjunction with label 2-after
  val L1aL2a = (b: BaselineLearner) => property(tokens) { x: Constituent =>
    labelOneAfter(b)(x) + "-" + labelTwoAfter(b)(x)
  }

  val isAScienceTerm = property(tokens) { x: Constituent =>
    //if (scienceTerms.contains(wordForm(x)))
    //  println(s"${wordForm(x)} is science term! ")
    scienceTerms.contains(wordForm(x))
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
    println("sentence = " + x.getTextAnnotation.getSentence(x.getSentenceId))
    println("x = " + x.getSurfaceForm)
    println("x.before = " + cons.getSurfaceForm)
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

  val chunkLabel = property(tokens) { x: Constituent =>
    val chunkView = constituentToAnnotationMap(x).questionTextAnnotation.getView(ViewNames.SHALLOW_PARSE)
    chunkView.getLabelsCovering(x).asScala.toList
  }
}
