package org.allenai.ari.models

/** Case class holding a question split into the question and answer parts.
  * @param question the non-answer portion of the question
  * @param keyAnswerPairs the list of multiple-choice answers. Will be empty for short answer
  * questions.
  */
case class SplitQuestion(question: String, keyAnswerPairs: Seq[(String, String)])
