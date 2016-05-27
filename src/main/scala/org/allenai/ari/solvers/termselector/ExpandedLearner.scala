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
    dataModel.afterWHword,
    dataModel.twoAfterWHword,
    dataModel.isAScienceTerm,
    dataModel.pos,
    dataModel.isItCapitalized,
    dataModel.lemma,
    dataModel.isItLastSentence,
    dataModel.isItCloseToEnd,
    dataModel.isItCloseToBeginning,
    dataModel.ner,
    dataModel.maxSalience,
    dataModel.sumSalience,
    dataModel.chunkLabel
  ) ++
    beforeAfterProperties(ViewNames.TOKENS) ++ beforeAfterProperties(ViewNames.POS) ++
    beforeAfterProperties(EssentialTermsConstants.VIEW_NAME) ++
    beforeAfterProperties(ViewNames.LEMMA) ++
    baselineProperties(expandedDataModel.baselineClassifiers.surfaceForm)
  override val logging = true

  private def baselineProperties(b: BaselineLearner): List[Property[Constituent]] = {
    List(
      dataModel.baselineTarget(b),
      dataModel.labelTwoBefore(b),
      dataModel.labelOneBefore(b),
      dataModel.labelOneAfter(b),
      dataModel.labelTwoAfter(b),
      dataModel.L2bL1b(b),
      dataModel.L1bL1a(b),
      dataModel.L1aL2a(b)
    )
  }

  private def beforeAfterProperties(view: String): List[Property[Constituent]] = {
    List(
      dataModel.afterWord("which", view),
      dataModel.twoAfterWord("which", view),
      dataModel.beforeWord("which", view),
      dataModel.twoBeforeWord("which", view),
      dataModel.afterWord("what", view),
      dataModel.twoAfterWord("what", view),
      dataModel.beforeWord("what", view),
      dataModel.twoBeforeWord("what", view),
      dataModel.afterWord("where", view),
      dataModel.twoAfterWord("where", view),
      dataModel.beforeWord("where", view),
      dataModel.twoBeforeWord("where", view),
      dataModel.afterWord("when", view),
      dataModel.twoAfterWord("when", view),
      dataModel.beforeWord("when", view),
      dataModel.twoBeforeWord("when", view),
      dataModel.afterWord("how", view),
      dataModel.twoAfterWord("how", view),
      dataModel.beforeWord("how", view),
      dataModel.twoBeforeWord("how", view)
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
