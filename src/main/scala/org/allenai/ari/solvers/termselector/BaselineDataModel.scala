package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent

/** TODO: add description */
class BaselineDataModel extends IllinoisDataModel {
  override val tokens = node[Constituent]

  override val goldLabel = property(tokens) { x: Constituent =>
    x.getTextAnnotation
      .getView(EssentialTermsConstants.VIEW_NAME)
      .getConstituentsCovering(x)
      .get(0)
      .getLabel
  }

  val wordForm = property(tokens, "wordForm", cache = true) { x: Constituent =>
    x.toString
  }
}
