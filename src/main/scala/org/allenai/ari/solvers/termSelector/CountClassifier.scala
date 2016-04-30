package org.allenai.ari.solvers.termselector

import edu.illinois.cs.cogcomp.lbj.pos.POSBaselineLearner

import scala.collection.JavaConverters._

class CountClassifier extends POSBaselineLearner {
  protected override def computePrediction(example: AnyRef): String = {
    val form = extractor.discreteValue(example)
    val countsMap = table.get(form)
    if (countsMap == null) {
      "UNKNOWN"
    } else {
      countsMap.asScala.toList.maxBy(_._2)._1
    }
  }
}
