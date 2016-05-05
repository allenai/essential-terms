import Dependencies._
import NativePackagerHelper.directory

name := "termselector"

description := "A pre-processing tool to identify important terms in questions"

serviceProjectSettings

setJvmMemory("10g")

libraryDependencies ++= Seq(
  allenAiCommon exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiGuice exclude("edu.stanford.nlp", "stanford-corenlp"),
  illinoisEdison,
  illinoisNlpPipeline,
  illinoisSaul,
  redisClient exclude("edu.stanford.nlp", "stanford-corenlp"),
  stanfordCorenlp331,  // different version than included in allenAiCommon
  stanfordModels331,  // different version than included in allenAiCommon
  word2VecJava exclude("edu.stanford.nlp", "stanford-corenlp"),
  wumpusClient exclude("edu.stanford.nlp", "stanford-corenlp")
)

fork := true
