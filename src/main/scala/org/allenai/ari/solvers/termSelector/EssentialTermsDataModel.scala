package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saul.datamodel.node.Node
import edu.illinois.cs.cogcomp.saul.datamodel.property.features.discrete.DiscreteProperty

/** A generic trait for data models for essential terms detection. */
trait EssentialTermsDataModel extends DataModel {

  /** A node containing all the constituents in the input data */
  def tokens: Node[Constituent]

  /** The actual label of the data: essential/not-essential */
  def goldLabel: DiscreteProperty[Constituent]
}
