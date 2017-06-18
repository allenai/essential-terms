import sbt.Keys._
import sbt._

name := "essential-terms"
description := "A pre-processing tool to identify important terms in questions"
scalaVersion := "2.11.5"
jvmMemory := "8g"

// Excludes the weka-stable from a dependency, since we use weka-dev.
def excludeWekaStable(module: ModuleID): ModuleID = {
  module.exclude("nz.ac.waikato.cms.weka", "weka-stable")
}

val illinoisEdison1 = excludeWekaStable("edu.illinois.cs.cogcomp" % "illinois-edison" % "3.0.47")
val illinoisNlpPipeline1 = excludeWekaStable(
  "edu.illinois.cs.cogcomp" % "illinois-nlp-pipeline" % "0.1.21"
)
val illinoisLBJava1 = excludeWekaStable("edu.illinois.cs.cogcomp" % "LBJava" % "1.2.20")
val illinoisSaul1 = excludeWekaStable("edu.illinois.cs.cogcomp" %% "saul" % "0.4")

val wispPlotting1 = "com.quantifind" %% "wisp" % "0.0.4"
val redisClient1 = "net.debasishg" %% "redisclient" % "3.0"

// specific versions needed for illinoisNlpPipeline
val stanfordCorenlp3311 = "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1"
val stanfordModels3311 = "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models"

val word2VecJava1 = "com.medallia.word2vec" % "Word2VecJava" % "0.10.3"

val allenAiCommonVersion = "1.4.6"
def allenAiCommonModule1(name: String) = "org.allenai.common" %% s"common-$name" % allenAiCommonVersion

val wumpusClient1 = "org.allenai.wumpus.client" % "wumpus-client-assembly" % "1.1.2"

libraryDependencies ++= Seq(
  allenAiCommonModule1("guice") exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiCommonModule1("core") exclude("edu.stanford.nlp", "stanford-corenlp"),
  wumpusClient1 exclude("edu.stanford.nlp", "stanford-corenlp"),
  illinoisEdison1,
  illinoisNlpPipeline1,
  illinoisSaul1,
  illinoisLBJava1,
  redisClient1,
  stanfordCorenlp3311,  // different version than included in allenAiCommon
  stanfordModels3311,  // different version than included in allenAiCommon
  word2VecJava1 exclude("edu.stanford.nlp", "stanford-corenlp"),
  wispPlotting1
)

val ai2PublicReleases = Resolver.bintrayRepo("allenai", "maven")
val cogcompReleases = "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"

resolvers ++= Seq(ai2PublicReleases, cogcompReleases)

unmanagedBase := baseDirectory.value / "lib"
includeFilter in unmanagedJars := "*.jar"
javaOptions ++= List("-Djava.library.path=lib", "-Xmx10g")
fork := true