package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.edison.features.factory._
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
  val confaltedPos = baselineDataModel.conflatedPos
  val posConjLemma = baselineDataModel.posConjLemma
  val posConjWordform = baselineDataModel.posConjWordform
  val posConjNer = baselineDataModel.posConjNer
  val lemmaConjNer = baselineDataModel.lemmaConjNer
  val wordFormConjNer = baselineDataModel.wordFormConjNer
  val wordFormConjNerConjPOS = baselineDataModel.wordFormConjNerConjPOS
  val conflatedPosConjLemma = baselineDataModel.conflatedPosConjLemma
  val conflatedPosConjWordform = baselineDataModel.conflatedPosConjWordform
  val conflatedPosConjNer = baselineDataModel.conflatedPosConjNer

  val deAdjectivalAbstractNounsSuffixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.deAdjectivalAbstractNounsSuffixes.getFeatures(x).asScala.mkString
  }

  val deNominalNounProducingSuffixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.deNominalNounProducingSuffixes.getFeatures(x).asScala.mkString
  }

  val deVerbalSuffix = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.deVerbalSuffix.getFeatures(x).asScala.mkString
  }

  // whether the word ends with an `-ing`.
  val gerundMarker = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.gerundMarker.getFeatures(x).asScala.mkString
  }

  val knownPrefixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.knownPrefixes.getFeatures(x).asScala.mkString
  }

  // An indicator for whether the word is a nominalization
  val nominalizationMarker = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.nominalizationMarker.getFeatures(x).asScala.mkString
  }

  // indicator for whether the word is a number
  val numberNormalizer = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.numberNormalizer.getFeatures(x).asScala.mkString
  }

  // The first and last two, three characters in the lower cased word
  val prefixSuffixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.prefixSuffixes.getFeatures(x).asScala.mkString
  }

  val wordnetExistsEntry = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.existsEntry).getFeatures(x).asScala.mkString
  }

  val wordnetSynsetsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synsetsFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetSynsetsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synsetsAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetLexicographerFileNamesAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.lexicographerFileNamesAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetLexicographerFileNamesFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.lexicographerFileNamesFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetHypernymFirstSenseLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymFirstSenseLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  val wordnetHypernymAllSensesLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymAllSensesLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  val wordnetHypernymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymsFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetHypernymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymsAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetMemberHolonymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.memberHolonymsAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetMemberHolonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.memberHolonymsFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetPartHolonymsFirstSenseLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsFirstSenseLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  val wordnetPartHolonymsAllSensesLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsAllSensesLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  val wordnetPartHolonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetPartHolonymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetPointersFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.pointersFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetPointersAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.pointersAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetSubstanceHolonymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.substanceHolonymsAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetSubstanceHolonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.substanceHolonymsFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetSynonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synonymsFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetSynonymsAllSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synonymsAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetVerbFramesAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.verbFramesAllSenses).getFeatures(x).asScala.mkString
  }

  val wordnetVerbFramesFirstSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.verbFramesFirstSense).getFeatures(x).asScala.mkString
  }

  val wordnetExistsConjFirstSynsetConjLexFileNamesAll = property(essentialTermTokens) { x: Constituent =>
    wordnetExistsEntry + wordnetSynsetsFirstSense(x) + wordnetLexicographerFileNamesAllSenses(x)
  }

  val xuPalmerFeature = property(essentialTermTokens) { x: Constituent =>
    SyntacticFrame.STANFORD.getFeatures(x).asScala.mkString
  }

  /** Given a constituent, this feature finds the corresponding node in the parse tree and reports
    * expands its parent, i.e. the expansion of the word's parent in the parse tree. The parse phrase corresponding
    * to the input constituent is marked in the feature.
    */
  val subcategoriationFeature = property(essentialTermTokens) { x: Constituent =>
    SubcategorizationFrame.STANFORD.getFeatures(x).asScala.mkString
  }

  val spanFeaturesUnorderedPosUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.POS_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesUnorderedPosBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.POS_BIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesUnorderedPosTrigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.POS_TRIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesUnorderedChunkUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.SHALLOW_PARSE_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesUnorderedChunkBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.SHALLOW_PARSE_BIGRAMS.getFeatures(x).asScala.mkString
  }

  // ordered
  val spanFeaturesOrderedPosUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.POS_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesOrderedPosBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.POS_BIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesOrderedPosTrigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.POS_TRIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesOrderedChunkUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.SHALLOW_PARSE_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  val spanFeaturesOrderedChunkBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.SHALLOW_PARSE_BIGRAMS.getFeatures(x).asScala.mkString
  }

  val rogetThesaurusFeatures = property(essentialTermTokens) { x: Constituent =>
    RogetThesaurusFeatures.INSTANCE.getFeatures(x).asScala.mkString
  }

  val parseSiblingsFeatures = property(essentialTermTokens) { x: Constituent =>
    ParseSiblings.STANFORD.getFeatures(x).asScala.mkString
  }

  val parsePhraseType = property(essentialTermTokens) { x: Constituent =>
    ParsePhraseType.STANFORD.getFeatures(x).asScala.mkString
  }

  val parsePhraseTypeOnly = property(essentialTermTokens) { x: Constituent =>
    ParsePhraseTypeOnly.STANFORD.getFeatures(x).asScala.mkString
  }

  val parsePath = property(essentialTermTokens) { x: Constituent =>
    ParsePath.STANFORD.getFeatures(x).asScala.mkString
  }

  val parseHeadWordPOS = property(essentialTermTokens) { x: Constituent =>
    ParseHeadWordPOS.STANFORD.getFeatures(x).asScala.mkString
  }

  val nomLexClassFeature = property(essentialTermTokens) { x: Constituent =>
    NomLexClassFeature.instance.getFeatures(x).asScala.mkString
  }

  val linearPosition = property(essentialTermTokens) { x: Constituent =>
    LinearPosition.instance.getFeatures(x).asScala.mkString
  }

  val dependencyPathUnigram = property(essentialTermTokens) { x: Constituent =>
    DependencyPathNgrams.STANFORD_UNIGRAM.getFeatures(x).asScala.mkString
  }

  val dependencyPathBigram = property(essentialTermTokens) { x: Constituent =>
    DependencyPathNgrams.STANFORD_BIGRAM.getFeatures(x).asScala.mkString
  }

  val corlexFeatureExtractor = property(essentialTermTokens) { x: Constituent =>
    CorlexFeatureExtractor.instance.getFeatures(x).asScala.mkString
  }

  val clauseFeatureExtractor = property(essentialTermTokens) { x: Constituent =>
    ClauseFeatureExtractor.STANFORD.getFeatures(x).asScala.mkString
  }

  val chunkPropertyFeatureFactoryHasModal = property(essentialTermTokens) { x: Constituent =>
    ChunkPropertyFeatureFactory.hasModalVerb.getFeatures(x).asScala.mkString
  }

  val chunkPropertyFeatureFactoryIsNegated = property(essentialTermTokens) { x: Constituent =>
    ChunkPropertyFeatureFactory.isNegated.getFeatures(x).asScala.mkString
  }

  val chunkPathPattern = property(essentialTermTokens) { x: Constituent =>
    ChunkPathPattern.SHALLOW_PARSE.getFeatures(x).asScala.mkString
  }

  val chunkEmbeddingShallowParse = property(essentialTermTokens) { x: Constituent =>
    ChunkEmbedding.SHALLOW_PARSE.getFeatures(x).asScala.mkString
  }

  val chunkEmbeddingNer = property(essentialTermTokens) { x: Constituent =>
    ChunkEmbedding.NER.getFeatures(x).asScala.mkString
  }

  val baselineTargetP = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    b(x)
  }

  val labelOrBaseline = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    if (b.isTraining) {
      goldLabel(x)
    } else if (b.classifier.observed(wordForm(x))) {
      b(x)
    } else {
      ""
    }
  }

  val labelOneBefore = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  val labelTwoBefore = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoBefore(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  val labelOneAfter = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentAfter(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  val labelTwoAfter = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
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

  val brownClusterFeatures = property(essentialTermTokens) { x: Constituent =>
    val a = brownClusterFeatureExtractor.getFeatures(x).asScala.toString
    println(a)
    a
  }
}
