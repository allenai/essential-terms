package org.allenai.ari.solvers.termselector.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** A set of parameters sufficient to create a learner
  * @param classifierType whether and how to identify and use essential terms in the model
  * @param classifierModel the type of the underlying model used for predictions
  * @param directAnswerQuestions
  */
class LearnerParams @Inject() (
    @Named("termselector.classifierType") val classifierType: String,
    @Named("termselector.classifierModel") val classifierModel: String,
    @Named("termselector.directAnswerQuestions") val directAnswerQuestions: Boolean
) {

}
