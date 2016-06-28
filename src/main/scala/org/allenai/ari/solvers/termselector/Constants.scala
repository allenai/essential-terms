package org.allenai.ari.solvers.termselector

/** Various constants related to essential term detection. */
protected case object Constants {
  val VIEW_NAME = "ESSENTIAL_TERMS"
  val IMPORTANT_LABEL = "IMPORTANT"
  val UNIMPORTANT_LABEL = "NOT-IMPORTANT"
  val LABEL_SEPARATOR = "*|*|*"

  // annotation keys
  val ANNOTATION_PREFIX = "AnnotationCache***"
  val SALIENCE_PREFIX = "***SalienceScore="
  val TOKENIZATION_PREFIX = "**essentialTermTokenization:"

  // cache files
  val SALIENCE_CACHE = "salienceCache.txt"

  // thresholds are chosen to maximize F1 on the dev set, given the threshold
  val LOOKUP_THRESHOLD = 0.5
  val MAX_SALIENCE_THRESHOLD = 0.02
  val SUM_SALIENCE_THRESHOLD = 0.07
  val LEMMA_BASELINE_THRESHOLD = 0.19
  val EXPANDED_LEARNER_THRESHOLD = 0.46
}
