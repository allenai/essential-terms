import Dependencies._
import NativePackagerHelper.directory

name := "termselector"

description := "A pre-processing tool to identify important terms in questions"

GlobalBuildSettings

ExtraMemJavaSettings

// UIUC Cogcomp software for NLP and ML tools used by solvers/termselector.
resolvers += "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"

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

javaOptions ++= List("-Xmx10g")
