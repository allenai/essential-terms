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
  "com.typesafe" % "config" % "1.3.0",
  "edu.illinois.cs.cogcomp" % "illinois-nlp-pipeline" % "0.1.17" withSources(),
  "edu.illinois.cs.cogcomp" % "illinois-edison" % "0.1.17" withSources(),
  "edu.illinois.cs.cogcomp" % "saul_2.11" % "0.1" withSources(),
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models"
)

resolvers ++= Seq("CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/")

fork := true

javaOptions ++= List("-Xmx10g")
