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

  // the threshold used in prediction of discrete values; these values are usually set by tuning.
  val EXPANDED_UP_THRESHOLD = 1.5
  val EXPANDED_DOWN_THRESHOLD = 0.5
  val MAX_SALIENCE_UP_THRESHOLD = 1.5
  val MAX_SALIENCE_DOWN_THRESHOLD = 0.5
  val SUM_SALIENCE_UP_THRESHOLD = 1.5
  val SUM_SALIENCE_DOWN_THRESHOLD = 0.5
  val WORD_BASELINE_UP_THRESHOLD = 1.5
  val WORD_BASELINE_DOWN_THRESHOLD = 0.5
}
