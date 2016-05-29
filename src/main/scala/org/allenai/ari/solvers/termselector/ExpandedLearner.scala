package org.allenai.ari.solvers.termselector

import org.allenai.common.Logging

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.lbjava.learn.SparseNetworkLearner
import edu.illinois.cs.cogcomp.saul.datamodel.property.Property

/** An expanded learner with a number of syntactic and semantic features. */
class ExpandedLearner(
    expandedDataModel: ExpandedDataModel
) extends IllinoisLearner(expandedDataModel) {

  // implement for trait EssentialTermsLearner
  override def dataModel = expandedDataModel

  // implement for trait Learnable[Constituent]
  override def label = dataModel.goldLabel
  override lazy val classifier = new SparseNetworkLearner

  override def feature = List(
    dataModel.wordForm,
    dataModel.lemma,
    dataModel.pos,
    dataModel.ner,
    dataModel.posConjNer,
    dataModel.lemmaConjNer,
    dataModel.wordFormConjNer,
    dataModel.wordFormConjNerConjPOS,
    dataModel.chunkLabel,
    dataModel.posConjWordform,
    dataModel.posConjLemma,
    dataModel.afterWHword,
    dataModel.afterWHwordWorForm,
    dataModel.twoAfterWHword,
    dataModel.isAScienceTerm,
    dataModel.isAScienceTermConjPos,
    dataModel.isAScienceTermLemma,
    dataModel.isItCapitalized,
    dataModel.isItLastSentence,
    dataModel.isItSecondToLastSentence,
    dataModel.isItCloseToEnd,
    dataModel.maxSalience,
    dataModel.sumSalience
  // brown cluster features
  // look into edison; should have good features
  // chunker
  ) ++ dataModel.baselinePropertiesPOSConjNerConjPos ++
    dataModel.baselinePropertiesPOSConjNer ++
    dataModel.baselinePropertiesPOSConjLemma ++
    dataModel.baselinePropertiesLemma ++
    dataModel.baselinePropertiesSurfaceForm ++
    beforeAfterPropertiesGivenView(ViewNames.TOKENS) ++
    beforeAfterPropertiesWHWords

  //    baselineProperties(expandedDataModel.baselineClassifiers.surfaceForm)
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
    loadSavedModel: Boolean
  ): (BaselineDataModel, BaselineLearners, ExpandedDataModel, ExpandedLearner) = {
    val (baselineDataModel, baselineLearners) = BaselineLearner.makeNewLearner(loadSavedModel)
    lazy val expandedDataModel = new ExpandedDataModel(baselineDataModel, baselineLearners)
    lazy val expandedLearner = new ExpandedLearner(expandedDataModel)
    if (loadSavedModel) {
      logger.debug(s"Loading ExpandedLearner model from ${expandedLearner.lcFilePath()}")
      expandedLearner.load()
    }
    (baselineDataModel, baselineLearners, expandedDataModel, expandedLearner)
  }
}
