package org.allenai.ari.solvers.termselector

import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn._
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property

/** An expanded learner with a number of syntactic and semantic features. */
class ExpandedLearner(
    expandedDataModel: ExpandedDataModel,
    classifierModel: String
) extends IllinoisLearner(expandedDataModel) {

  // implement for trait EssentialTermsLearner
  override def dataModel = expandedDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel

  override lazy val classifier = classifierModel match {
    case "SVM" => new SupportVectorMachine()
    case "NaiveBayes" => new NaiveBayes()
    case "SparseNetwork" => new SparseNetworkLearner()
    case _ => new SupportVectorMachine()
  }

  //  classifier.getParameters.asInstanceOf[SupportVectorMachine.Parameters].solverType = "L2_LR"

  override def feature = List(
    dataModel.wordForm,
    dataModel.lemma,
    dataModel.baselinePropertiesLemmaPairSingleLabel,
    dataModel.posConjNer,
    dataModel.wordFormConjNer,
    dataModel.wordFormConjNerConjPOS,
    dataModel.chunkLabel,
    dataModel.posConjWordform,
    dataModel.posConjLemma,
    dataModel.conflatedPosConjLemma,
    dataModel.conflatedPosConjWordform,
    dataModel.deVerbalSuffix,
    dataModel.gerundMarker,
    dataModel.knownPrefixes,
    dataModel.nominalizationMarker,
    dataModel.numberNormalizer,
    dataModel.deVerbalSuffix,
    dataModel.prefixSuffixes,
    dataModel.parsePath,
    dataModel.brownClusterFeatures,
    dataModel.dependencyPathUnigram,
    dataModel.dependencyPathBigram,
    dataModel.wordnetSynsetsFirstSense,
    dataModel.wordnetSynsetsAllSenses,
    dataModel.wordnetLexicographerFileNamesAllSenses,
    dataModel.wordnetLexicographerFileNamesFirstSense,
    dataModel.wordnetHypernymFirstSenseLexicographerFileNames,
    dataModel.wordnetHypernymAllSensesLexicographerFileNames,
    dataModel.wordnetPointersFirstSense,
    dataModel.wordnetPointersAllSenses,
    dataModel.wordnetSynonymsFirstSense,
    dataModel.wordnetSynonymsAllSense,
    dataModel.wordnetExistsConjFirstSynsetConjLexFileNamesAll,
    dataModel.isAScienceTerm,
    dataModel.isAScienceTermLemma,
    dataModel.isItCapitalized,
    dataModel.isItLastSentence,
    dataModel.maxSalience,
    dataModel.sumSalience
  //    dataModel.parsePhraseType,
  //    dataModel.parseHeadWordPOS,
  //    dataModel.nomLexClassFeature,
  //    dropped at feature selection
  //    dataModel.pos,
  //    dataModel.confaltedPos,
  //    dataModel.ner,
  //    dataModel.lemmaConjNer,
  //    dataModel.wordnetVerbFramesAllSenses, // not helped much
  //    dataModel.wordnetVerbFramesFirstSenses // not helped much
  //    dataModel.wordnetExistsEntry // not helped much
  //    dataModel.corlexFeatureExtractor // data not loaded
  //    dataModel.clauseFeatureExtractor // not helped much
  //    dataModel.chunkPropertyFeatureFactoryHasModal, // not helped much
  //    dataModel.chunkPropertyFeatureFactoryIsNegated // not helped much
  //    dataModel.conflatedPosConjNer,
  //    dataModel.deAdjectivalAbstractNounsSuffixes,
  //    dataModel.deNominalNounProducingSuffixes
  //    dataModel.xuPalmerFeature,
  //    dataModel.chunkPathPattern,
  //    dataModel.wordnetHypernymsFirstSense,
  //    dataModel.wordnetHypernymsAllSenses,
  //    dataModel.wordnetMemberHolonymsAllSenses,
  //    dataModel.wordnetMemberHolonymsFirstSense,
  //    dataModel.wordnetPartHolonymsAllSensesLexicographerFileNames,
  //    dataModel.wordnetPartHolonymsFirstSenseLexicographerFileNames,
  //    dataModel.wordnetPartHolonymsFirstSense,
  //    dataModel.wordnetPartHolonymsAllSenses,
  //    dataModel.wordnetSubstanceHolonymsAllSenses,
  //    dataModel.wordnetSubstanceHolonymsFirstSense,
  //    dataModel.subcategoriationFeature,
  //    dataModel.spanFeaturesUnorderedChunkBigram,
  //    dataModel.spanFeaturesUnorderedPosBigram,
  //    dataModel.spanFeaturesUnorderedPosTrigram,
  //    dataModel.spanFeaturesUnorderedChunkUnigram,
  //    dataModel.spanFeaturesUnorderedChunkBigram,
  //    dataModel.spanFeaturesOrderedChunkBigram,
  //    dataModel.spanFeaturesOrderedPosBigram,
  //    dataModel.spanFeaturesOrderedPosTrigram,
  //    dataModel.spanFeaturesOrderedChunkUnigram,
  //    dataModel.spanFeaturesOrderedChunkBigram,
  //    dataModel.rogetThesaurusFeatures,
  //    dataModel.parseSiblingsFeatures,
  //    dataModel.parsePhraseTypeOnly,
  //    dataModel.chunkEmbeddingShallowParse,
  //    dataModel.chunkEmbeddingNer
  //    dataModel.afterWHword,
  //    dataModel.afterWHwordWorForm,
  //    dataModel.twoAfterWHword,
  //    dataModel.isItSecondToLastSentence,
  //    dataModel.isItCloseToEnd,
  //      dataModel.isAScienceTermConjPos,
  ) ++ dataModel.baselinePropertiesSurfaceConjNerConjPos ++
    dataModel.baselinePropertiesPOSConjNer ++
    dataModel.baselinePropertiesPOSConjLemma ++
    dataModel.baselinePropertiesLemma ++
    dataModel.baselinePropertiesSurfaceForm ++
    beforeAfterPropertiesGivenView(ViewNames.TOKENS) ++
    beforeAfterPropertiesWHWords ++
    dataModel.baselinePropertiesLemmaPair

  override val logging = true

  private def beforeAfterPropertiesGivenWord(view: String, word: String): List[Property[Constituent]] = {
    List(
      dataModel.afterWord(word, view),
      dataModel.twoAfterWord(word, view),
      dataModel.beforeWord(word, view),
      dataModel.twoBeforeWord(word, view)
    )
  }

  private def beforeAfterPropertiesWHWords(): List[Property[Constituent]] = {
    beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "which") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "what") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "where") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "when") ++
      beforeAfterPropertiesGivenWord(ViewNames.TOKENS, "how")
  }

  private def beforeAfterPropertiesGivenView(view: String): List[Property[Constituent]] = {
    List(
      dataModel.wordAfter(view),
      dataModel.wordTwoAfter(view),
      dataModel.wordBefore(view),
      dataModel.wordTwoBefore(view)
    )
  }
}

object ExpandedLearner extends Logging {

  /** Make a new ExpandedLearner; also return the underlying BaselineLearner and data models.
    *
    * @param loadSavedModel whether to load a previously saved model
    */
  def makeNewLearner(
    loadSavedModel: Boolean,
    classifierModel: String
  ): (BaselineDataModel, BaselineLearners, ExpandedDataModel, ExpandedLearner) = {
    val (baselineDataModel, baselineLearners) = BaselineLearner.makeNewLearners(loadSavedModel)
    lazy val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners)
    lazy val expandedLearner = new ExpandedLearner(expandedDataModel, classifierModel)
    expandedLearner.modelSuffix = classifierModel
    if (loadSavedModel) {
      logger.debug(s"Loading ExpandedLearner model from ${expandedLearner.lcFilePath}")
      expandedLearner.load()
    }
    (baselineDataModel, baselineLearners, expandedDataModel, expandedLearner)
  }
}
