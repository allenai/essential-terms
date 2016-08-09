package org.allenai.ari.solvers.termselector.learners

import org.allenai.ari.solvers.termselector.Sensors

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.saul.datamodel.DataModel
import edu.illinois.cs.cogcomp.saul.datamodel.node.Node
import edu.illinois.cs.cogcomp.saul.datamodel.property.features.discrete.DiscreteProperty

/** A generic trait for data models for essential terms detection. */
trait IllinoisDataModel extends DataModel {

  /** A node containing all the non-stopword constituents in the input data */
  def essentialTermTokens: Node[Constituent]

  /** The actual label of the data: essential/not-essential */
  def goldLabel: DiscreteProperty[Constituent]

  def sensors: Sensors
}
