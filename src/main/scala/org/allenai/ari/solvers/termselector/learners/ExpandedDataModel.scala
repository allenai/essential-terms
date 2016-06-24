package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.{ Constants, EssentialTermsSensors }

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Sentence, Constituent }
import edu.illinois.cs.cogcomp.edison.features.factory._
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property

import scala.collection.JavaConverters._

/** This class contains details about definiitions of the main properties and how they are defined based upon the main
  * inputs of the problem
  */
class ExpandedDataModel(
    baselineDataModel: BaselineDataModel,
    baselineClassifiers: BaselineLearners,
    salienceBaselines: SalienceLearners
) extends IllinoisDataModel {

  // first copy relevant fields from BaselineDataModel and the underlying (mutable) DataModel
  nodes ++= baselineDataModel.nodes
  edges ++= baselineDataModel.edges
  properties ++= baselineDataModel.properties
  //propertyCacheList ++= baselineDataModel.propertyCacheList
  override val essentialTermTokens = baselineDataModel.essentialTermTokens
  val sentences = node[Sentence]

  val tokenToSentence = edge(essentialTermTokens, sentences)
  tokenToSentence.addSensor({ (c: Constituent) => c.getTextAnnotation.getSentence(c.getSentenceId) })

  // properties
  // TODO: transfer all these properties here automatically?
  override val goldLabel = baselineDataModel.goldLabel
  // used in the regression model
  val goldRealConfidence = property(essentialTermTokens) { x: Constituent =>
    x.getTextAnnotation
      .getView(Constants.VIEW_NAME)
      .getConstituentsCovering(x)
      .get(0).getConstituentScore
  }
  private val wordForm = baselineDataModel.wordForm
  private val lemma = baselineDataModel.lemma
  private val ner = baselineDataModel.ner
  private val pos = baselineDataModel.pos
  private val confaltedPos = baselineDataModel.conflatedPos
  private val posConjLemma = baselineDataModel.posConjLemma
  private val posConjWordform = baselineDataModel.posConjWordform
  private val posConjNer = baselineDataModel.posConjNer
  private val lemmaConjNer = baselineDataModel.lemmaConjNer
  private val wordFormConjNer = baselineDataModel.wordFormConjNer
  private val wordFormConjNerConjPOS = baselineDataModel.wordFormConjNerConjPOS
  private val conflatedPosConjLemma = baselineDataModel.conflatedPosConjLemma
  private val conflatedPosConjWordform = baselineDataModel.conflatedPosConjWordform
  private val conflatedPosConjNer = baselineDataModel.conflatedPosConjNer
  private val maxSalience = baselineDataModel.maxSalience
  private val sumSalience = baselineDataModel.sumSalience

  private val deAdjectivalAbstractNounsSuffixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.deAdjectivalAbstractNounsSuffixes.getFeatures(x).asScala.mkString
  }

  private val deNominalNounProducingSuffixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.deNominalNounProducingSuffixes.getFeatures(x).asScala.mkString
  }

  private val deVerbalSuffix = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.deVerbalSuffix.getFeatures(x).asScala.mkString
  }

  // whether the word ends with an `-ing`.
  private val gerundMarker = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.gerundMarker.getFeatures(x).asScala.mkString
  }

  private val knownPrefixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.knownPrefixes.getFeatures(x).asScala.mkString
  }

  // An indicator for whether the word is a nominalization
  private val nominalizationMarker = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.nominalizationMarker.getFeatures(x).asScala.mkString
  }

  // indicator for whether the word is a number
  private val numberNormalizer = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.numberNormalizer.getFeatures(x).asScala.mkString
  }

  // The first and last two, three characters in the lower cased word
  private val prefixSuffixes = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.prefixSuffixes.getFeatures(x).asScala.mkString
  }

  private val wordnetExistsEntry = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.existsEntry).getFeatures(x).asScala.mkString
  }

  private val wordnetSynsetsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synsetsFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetSynsetsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synsetsAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetLexicographerFileNamesAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.lexicographerFileNamesAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetLexicographerFileNamesFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.lexicographerFileNamesFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetHypernymFirstSenseLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymFirstSenseLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  private val wordnetHypernymAllSensesLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymAllSensesLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  private val wordnetHypernymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymsFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetHypernymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.hypernymsAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetMemberHolonymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.memberHolonymsAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetMemberHolonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.memberHolonymsFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetPartHolonymsFirstSenseLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsFirstSenseLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  private val wordnetPartHolonymsAllSensesLexicographerFileNames = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsAllSensesLexicographerFileNames).getFeatures(x).asScala.mkString
  }

  private val wordnetPartHolonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetPartHolonymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.partHolonymsAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetPointersFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.pointersFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetPointersAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.pointersAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetSubstanceHolonymsAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.substanceHolonymsAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetSubstanceHolonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.substanceHolonymsFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetSynonymsFirstSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synonymsFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetSynonymsAllSense = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.synonymsAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetVerbFramesAllSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.verbFramesAllSenses).getFeatures(x).asScala.mkString
  }

  private val wordnetVerbFramesFirstSenses = property(essentialTermTokens) { x: Constituent =>
    WordFeatureExtractorFactory.getWordNetFeatureExtractor(WordNetFeatureExtractor.WordNetFeatureClass.verbFramesFirstSense).getFeatures(x).asScala.mkString
  }

  private val wordnetExistsConjFirstSynsetConjLexFileNamesAll = property(essentialTermTokens) { x: Constituent =>
    wordnetExistsEntry + wordnetSynsetsFirstSense(x) + wordnetLexicographerFileNamesAllSenses(x)
  }

  private val xuPalmerFeature = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.DEPENDENCY_STANFORD).getConstituentsCovering(x)
    y.asScala.map { SyntacticFrame.STANFORD.getFeatures(_).asScala.mkString }.mkString("*")
  }

  /** Given a constituent, this feature finds the corresponding node in the parse tree and reports
    * expands its parent, i.e. the expansion of the word's parent in the parse tree. The parse phrase corresponding
    * to the input constituent is marked in the feature.
    */
  private val subcategoriationFeature = property(essentialTermTokens) { x: Constituent =>
    SubcategorizationFrame.STANFORD.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesUnorderedPosUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.POS_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesUnorderedPosBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.POS_BIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesUnorderedPosTrigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.POS_TRIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesUnorderedChunkUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.SHALLOW_PARSE_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesUnorderedChunkBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesUnordered.SHALLOW_PARSE_BIGRAMS.getFeatures(x).asScala.mkString
  }

  // ordered
  private val spanFeaturesOrderedPosUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.POS_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesOrderedPosBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.POS_BIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesOrderedPosTrigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.POS_TRIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesOrderedChunkUnigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.SHALLOW_PARSE_UNIGRAMS.getFeatures(x).asScala.mkString
  }

  private val spanFeaturesOrderedChunkBigram = property(essentialTermTokens) { x: Constituent =>
    SpanFeaturesOrdered.SHALLOW_PARSE_BIGRAMS.getFeatures(x).asScala.mkString
  }

  private val rogetThesaurusFeatures = property(essentialTermTokens) { x: Constituent =>
    RogetThesaurusFeatures.INSTANCE.getFeatures(x).asScala.mkString
  }

  private val parseSiblingsFeatures = property(essentialTermTokens) { x: Constituent =>
    ParseSiblings.STANFORD.getFeatures(x).asScala.mkString
  }

  private val parsePhraseType = property(essentialTermTokens) { x: Constituent =>
    ParsePhraseType.STANFORD.getFeatures(x).asScala.mkString
  }

  private val parsePhraseTypeOnly = property(essentialTermTokens) { x: Constituent =>
    ParsePhraseTypeOnly.STANFORD.getFeatures(x).asScala.mkString
  }

  private val parsePath = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.DEPENDENCY_STANFORD).getConstituentsCovering(x)
    y.asScala.map { ParsePath.STANFORD.getFeatures(_).asScala.mkString }.mkString("*")
  }

  private val parseHeadWordPOS = property(essentialTermTokens) { x: Constituent =>
    ParseHeadWordPOS.STANFORD.getFeatures(x).asScala.mkString
  }

  private val nomLexClassFeature = property(essentialTermTokens) { x: Constituent =>
    NomLexClassFeature.instance.getFeatures(x).asScala.mkString
  }

  private val dependencyPathUnigram = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.DEPENDENCY_STANFORD).getConstituentsCovering(x)
    y.asScala.map { DependencyPathNgrams.STANFORD_UNIGRAM.getFeatures(_).asScala.mkString }.mkString("*")
  }

  private val dependencyPathBigram = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.DEPENDENCY_STANFORD).getConstituentsCovering(x)
    y.asScala.map { DependencyPathNgrams.STANFORD_BIGRAM.getFeatures(_).asScala.mkString }.mkString("*")
  }

  private val corlexFeatureExtractor = property(essentialTermTokens) { x: Constituent =>
    CorlexFeatureExtractor.instance.getFeatures(x).asScala.mkString
  }

  private val clauseFeatureExtractor = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.DEPENDENCY_STANFORD).getConstituentsCovering(x)
    y.asScala.map { ClauseFeatureExtractor.STANFORD.getFeatures(_).asScala.mkString }.mkString("*")
  }

  private val chunkPropertyFeatureFactoryHasModal = property(essentialTermTokens) { x: Constituent =>
    ChunkPropertyFeatureFactory.hasModalVerb.getFeatures(x).asScala.mkString
  }

  private val chunkPropertyFeatureFactoryIsNegated = property(essentialTermTokens) { x: Constituent =>
    ChunkPropertyFeatureFactory.isNegated.getFeatures(x).asScala.mkString
  }

  private val chunkPathPattern = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.DEPENDENCY_STANFORD).getConstituentsCovering(x)
    y.asScala.map { ChunkPathPattern.SHALLOW_PARSE.getFeatures(_).asScala.mkString }.mkString("*")
  }

  private val chunkEmbeddingShallowParse = property(essentialTermTokens) { x: Constituent =>
    ChunkEmbedding.SHALLOW_PARSE.getFeatures(x).asScala.mkString
  }

  private val chunkEmbeddingNer = property(essentialTermTokens) { x: Constituent =>
    ChunkEmbedding.NER.getFeatures(x).asScala.mkString
  }

  private val baselineLabelWithThreshold = (b: IllinoisLearner, ths: Seq[Double]) => property(essentialTermTokens) {
    x: Constituent => ths.map { th => b.predictLabel(x, th) }.toList
  }

  val baselineTargetP = (b: BaselineLearner) => property(essentialTermTokens, cache = true) { x: Constituent =>
    b(x)
  }

  val baselineConfidence = (b: IllinoisLearner) => property(essentialTermTokens) { x: Constituent =>
    b.predictProbOfBeingEssential(x)
  }

  private val labelOrBaseline = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    if (b.isTraining) {
      goldLabel(x)
    } else if (b.classifier.observed(wordForm(x))) {
      b(x)
    } else {
      ""
    }
  }

  private val labelOneBefore = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  private val labelTwoBefore = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoBefore(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) {
      if (b.isTraining) goldLabel(cons) else b(cons)
    } else {
      ""
    }
  }

  private val labelOneAfter = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentAfter(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  private val labelTwoAfter = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoAfter(y.head)
    // make sure the spans are different. Otherwise it is not valid
    if (cons.getSpan != x.getSpan) labelOrBaseline(b)(cons) else ""
  }

  // label 2-before conjunction with label 1-before
  private val L2bL1b = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    labelTwoBefore(b)(x) + "-" + labelOneBefore(b)(x)
  }

  // label 1-before conjunction with label 1-after
  private val L1bL1a = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    labelOneBefore(b)(x) + "-" + labelOneAfter(b)(x)
  }

  // label 1-after conjunction with label 2-after
  private val L1aL2a = (b: BaselineLearner) => property(essentialTermTokens) { x: Constituent =>
    labelOneAfter(b)(x) + "-" + labelTwoAfter(b)(x)
  }

  val baselineConfidenceScores: List[Property[Constituent]] = {
    List(
      baselineConfidence(baselineClassifiers.surfaceForm),
      baselineConfidence(baselineClassifiers.lemma),
      baselineConfidence(baselineClassifiers.baselineLearnerLemmaPair),
      baselineConfidence(baselineClassifiers.posConjLemma),
      baselineConfidence(baselineClassifiers.wordFormConjNer),
      baselineConfidence(baselineClassifiers.wordFormConjNerConjPos),
      baselineConfidence(salienceBaselines.max),
      baselineConfidence(salienceBaselines.sum)
    )
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

  // group of baselines cut in different thresholds
  private val baselinesWithThresholds = {
    List(
      baselineLabelWithThreshold(baselineClassifiers.lemma, Seq(0.73, 0.63, 0.54, 0.46, 0.38, 0.37, 0.31, 0.24, 0.19)),
      baselineLabelWithThreshold(baselineClassifiers.surfaceForm, Seq(0.72, 0.60, 0.46, 0.37, 0.35, 0.33, 0.24, 0.19)),
      baselineLabelWithThreshold(salienceBaselines.max, Seq(0.18, 0.10, 0.08, 0.04, 0.02, 0.01)),
      baselineLabelWithThreshold(salienceBaselines.sum, Seq(0.39, 0.37, 0.26, 0.09, 0.06, 0.03))
    )
  }

  private val baselinePropertiesSurfaceForm = baselineProperties(baselineClassifiers.surfaceForm)

  private val baselinePropertiesLemma = baselineProperties(baselineClassifiers.lemma)

  private val baselinePropertiesLemmaPair = baselineProperties(baselineClassifiers.baselineLearnerLemmaPair)

  private val baselinePropertiesLemmaPairSingleLabel = property(essentialTermTokens) { x: Constituent =>
    val labelPair = baselineClassifiers.baselineLearnerLemmaPair(x)
    labelPair.split(s"[${Constants.LABEL_SEPARATOR}]").last
  }

  private val baselinePropertiesPOSConjLemma = baselineProperties(baselineClassifiers.posConjLemma)

  private val baselinePropertiesPOSConjNer = baselineProperties(baselineClassifiers.wordFormConjNer)

  private val baselinePropertiesSurfaceConjNerConjPos = baselineProperties(baselineClassifiers.wordFormConjNerConjPos)

  private val isAScienceTerm = property(essentialTermTokens) { x: Constituent =>
    EssentialTermsSensors.scienceTerms.contains(wordForm(x))
  }

  private val isAScienceTermConjPos = property(essentialTermTokens) { x: Constituent =>
    EssentialTermsSensors.scienceTerms.contains(wordForm(x)) + pos(x)
  }

  private val isAScienceTermLemma = property(essentialTermTokens) { x: Constituent =>
    EssentialTermsSensors.scienceTerms.contains(lemma(x))
  }

  private val srlLabel = property(essentialTermTokens) { x: Constituent =>
    val y = x.getTextAnnotation.getView(ViewNames.SRL_VERB).getConstituentsCovering(x)
    y.asScala.map(_.getLabel).mkString("*")
  }

  private val isItCapitalized = property(essentialTermTokens) { x: Constituent =>
    val word = wordForm(x)
    val firstCharacter: String = word.substring(0, 1)
    val upperCase: String = firstCharacter.toUpperCase
    upperCase.matches("[A-Z]") && (upperCase == firstCharacter)
  }

  private val whKeyWords = Set("which", "what", "where", "when", "how")

  private val afterWHword = property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    whKeyWords.contains(wordForm(cons).toLowerCase)
  }

  private val afterWHwordWorForm = property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentBefore(y.head)
    if (whKeyWords.contains(wordForm(cons).toLowerCase)) wordForm(x) else ""
  }

  private val twoAfterWHword = property(essentialTermTokens) { x: Constituent =>
    val y = EssentialTermsSensors.getConstituentCoveringInView(x, ViewNames.TOKENS).asScala
    val cons = EssentialTermsSensors.getConstituentTwoBefore(y.head)
    whKeyWords.contains(wordForm(cons).toLowerCase)
  }

  // returns the word after the given word in the given view
  private val wordAfter = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = EssentialTermsSensors.getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentAfter(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  // checks whether the wordAfter in the given view is the same as the word given in the input.
  private val afterWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordAfter(view)(x).toString == word
    }
  }

  private val wordTwoAfter = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = EssentialTermsSensors.getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentTwoAfter(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  private val twoAfterWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordTwoAfter(view)(x).toString == word
    }
  }

  private val wordBefore = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = EssentialTermsSensors.getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentBefore(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  private val beforeWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordBefore(view)(x).toString == word
    }
  }

  private val wordTwoBefore = { (view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      val y = EssentialTermsSensors.getConstituentCoveringInView(x, view).asScala
      val c = EssentialTermsSensors.getConstituentTwoBefore(y.head, view)
      if (view == ViewNames.TOKENS) c.getSurfaceForm else c.getLabel
    }
  }

  private val twoBeforeWord = { (word: String, view: String) =>
    property(essentialTermTokens) { x: Constituent =>
      wordTwoBefore(view)(x).toString == word
    }
  }

  private val isItLastSentence = property(essentialTermTokens) { x: Constituent =>
    x.getSentenceId == (x.getTextAnnotation.getNumberOfSentences - 1)
  }

  private val isItSecondToLastSentence = property(essentialTermTokens) { x: Constituent =>
    x.getSentenceId == (x.getTextAnnotation.getNumberOfSentences - 2)
  }

  private val isItCloseToEnd = property(essentialTermTokens) { x: Constituent =>
    x.getSentenceId / x.getTextAnnotation.getNumberOfSentences
  }

  private val w2v = property(essentialTermTokens, cache = true) { x: Constituent =>
    val key = if (EssentialTermsSensors.w2vModel.forSearch().contains(wordForm(x))) {
      wordForm(x)
    } else {
      EssentialTermsSensors.w2vNoMatchStr
    }
    EssentialTermsSensors.w2vModel.forSearch().getRawVector(key).asScala.map(_.doubleValue()).toList.head
  }

  private val chunkLabel = property(essentialTermTokens) { x: Constituent =>
    val chunkView = EssentialTermsSensors.constituentToAnnotationMap(x).questionTextAnnotation
      .getView(ViewNames.SHALLOW_PARSE)
    chunkView.getLabelsCovering(x).asScala.toList
  }

  private val brownClusterFeatures = property(essentialTermTokens) { x: Constituent =>
    EssentialTermsSensors.brownClusterFeatureExtractor.getFeatures(x).asScala.toString
  }

  private def beforeAfterPropertiesGivenWord(view: String, word: String): List[Property[Constituent]] = {
    List(
      afterWord(word, view),
      twoAfterWord(word, view),
      beforeWord(word, view),
      twoBeforeWord(word, view)
    )
  }

  private val beforeAfterPropertiesWHWords: List[Property[Constituent]] = {
    beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "which") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "what") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "where") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "when") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "how")
  }

  private val beforeAfterPropertiesGivenViewTokens = beforeAfterPropertiesGivenView(ViewNames.TOKENS)

  private def beforeAfterPropertiesGivenView(view: String): List[Property[Constituent]] = {
    List(
      wordAfter(view),
      wordTwoAfter(view),
      wordBefore(view),
      wordTwoBefore(view)
    )
  }

  private val sparseProperties: List[Property[Constituent]] = {
    List(
      wordForm,
      lemma,
      wordFormConjNer,
      wordFormConjNerConjPOS,
      posConjWordform,
      posConjLemma,
      conflatedPosConjLemma,
      conflatedPosConjWordform,
      dependencyPathUnigram,
      dependencyPathBigram,
      wordnetSynsetsFirstSense,
      wordnetSynsetsAllSenses,
      wordnetLexicographerFileNamesAllSenses,
      wordnetLexicographerFileNamesFirstSense,
      wordnetHypernymFirstSenseLexicographerFileNames,
      wordnetHypernymAllSensesLexicographerFileNames,
      wordnetPointersFirstSense,
      wordnetPointersAllSenses,
      wordnetSynonymsFirstSense,
      wordnetSynonymsAllSense,
      wordnetExistsConjFirstSynsetConjLexFileNamesAll
    ) ++ beforeAfterPropertiesGivenViewTokens
  }

  private val denseProperties: List[Property[Constituent]] = {
    List(
      baselinePropertiesLemmaPairSingleLabel,
      posConjNer,
      chunkLabel,
      deVerbalSuffix,
      gerundMarker,
      knownPrefixes,
      nominalizationMarker,
      numberNormalizer,
      prefixSuffixes,
      parsePath,
      brownClusterFeatures,
      isAScienceTerm,
      isAScienceTermLemma,
      isItCapitalized,
      isItLastSentence
    ) ++
      baselinePropertiesSurfaceConjNerConjPos ++
      baselinePropertiesPOSConjNer ++
      baselinePropertiesPOSConjLemma ++
      baselinePropertiesLemma ++
      baselinePropertiesSurfaceForm ++
      beforeAfterPropertiesWHWords ++
      baselinePropertiesLemmaPair ++
      baselinesWithThresholds ++
      baselineConfidenceScores
  }

  val allProperties: List[Property[Constituent]] = {
    denseProperties ++ sparseProperties
  }

  // features commented out due to poor effect
  //    parsePhraseType,
  //    parseHeadWordPOS,
  //    nomLexClassFeature,
  //    dropped at feature selection
  //    pos,
  //    confaltedPos,
  //    ner,
  //    lemmaConjNer,
  //    wordnetVerbFramesAllSenses, // not helped much
  //    wordnetVerbFramesFirstSenses // not helped much
  //    wordnetExistsEntry // not helped much
  //    corlexFeatureExtractor // data not loaded
  //    clauseFeatureExtractor // not helped much
  //    chunkPropertyFeatureFactoryHasModal, // not helped much
  //    chunkPropertyFeatureFactoryIsNegated // not helped much
  //    conflatedPosConjNer,
  //    deAdjectivalAbstractNounsSuffixes,
  //    deNominalNounProducingSuffixes
  //    xuPalmerFeature,
  //    chunkPathPattern,
  //    wordnetHypernymsFirstSense,
  //    wordnetHypernymsAllSenses,
  //    wordnetMemberHolonymsAllSenses,
  //    wordnetMemberHolonymsFirstSense,
  //    wordnetPartHolonymsAllSensesLexicographerFileNames,
  //    wordnetPartHolonymsFirstSenseLexicographerFileNames,
  //    wordnetPartHolonymsFirstSense,
  //    wordnetPartHolonymsAllSenses,
  //    wordnetSubstanceHolonymsAllSenses,
  //    wordnetSubstanceHolonymsFirstSense,
  //    subcategoriationFeature,
  //    spanFeaturesUnorderedChunkBigram,
  //    spanFeaturesUnorderedPosBigram,
  //    spanFeaturesUnorderedPosTrigram,
  //    spanFeaturesUnorderedChunkUnigram,
  //    spanFeaturesUnorderedChunkBigram,
  //    spanFeaturesOrderedChunkBigram,
  //    spanFeaturesOrderedPosBigram,
  //    spanFeaturesOrderedPosTrigram,
  //    spanFeaturesOrderedChunkUnigram,
  //    spanFeaturesOrderedChunkBigram,
  //    rogetThesaurusFeatures,
  //    parseSiblingsFeatures,
  //    parsePhraseTypeOnly,
  //    chunkEmbeddingShallowParse,
  //    chunkEmbeddingNer
  //    afterWHword,
  //    afterWHwordWorForm,
  //    twoAfterWHword,
  //    isItSecondToLastSentence,
  //    isItCloseToEnd,
  //     isAScienceTermConjPos,
}
