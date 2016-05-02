import Dependencies._
import NativePackagerHelper.directory

name := "termselector"

description := "A pre-processing tool to identify important terms in questions"

GlobalBuildSettings

ExtraMemJavaSettings

libraryDependencies ++= Seq(
  allenAiCommon exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiGuice exclude("edu.stanford.nlp", "stanford-corenlp"),
  redisClient exclude("edu.stanford.nlp", "stanford-corenlp"),
  word2VecJava exclude("edu.stanford.nlp", "stanford-corenlp"),
  wumpusClient exclude("edu.stanford.nlp", "stanford-corenlp"),
  stanfordCorenlp331,
  stanfordModels331,
  illinoisNlpPipeline,
  illinoisEdison,
  illinoisSaul
)

fork := true

javaOptions ++= List("-Xmx10g")
