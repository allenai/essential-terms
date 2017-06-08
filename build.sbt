import sbt.Keys._
import sbt._

name := "essential-terms"
description := "A pre-processing tool to identify important terms in questions"
scalaVersion := "2.11.5"

// Excludes the weka-stable from a dependency, since we use weka-dev.
def excludeWekaStable(module: ModuleID): ModuleID = {
  module.exclude("nz.ac.waikato.cms.weka", "weka-stable")
}
// specific versions needed for illinoisNlpPipeline
val stanfordCorenlp331 = "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1"
val stanfordModels331 = "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models"

libraryDependencies ++= Seq(
  excludeWekaStable("edu.illinois.cs.cogcomp" % "illinois-edison" % "3.0.47"),
  excludeWekaStable("edu.illinois.cs.cogcomp" % "illinois-nlp-pipeline" % "0.1.21"),
  excludeWekaStable("edu.illinois.cs.cogcomp" % "saul_2.11" % "0.4"),
  excludeWekaStable("edu.illinois.cs.cogcomp" % "LBJava" % "1.2.20"),
  "net.debasishg" %% "redisclient" % "3.0",
  "org.allenai.aristo" %% "rephrase-core" % "0.0.6",
  stanfordCorenlp331,  // different version than included in allenAiCommon
  stanfordModels331,  // different version than included in allenAiCommon
  ("com.medallia.word2vec" % "Word2VecJava" % "0.10.3").exclude("edu.stanford.nlp", "stanford-corenlp"),
  ("org.allenai.wumpus" %% "wumpus-client" % "1.1.1").exclude("edu.stanford.nlp", "stanford-corenlp"),
  "com.quantifind" %% "wisp" % "0.0.4"
)

val ai2PublicReleases = Resolver.bintrayRepo("allenai", "maven")
val cogcompReleases = "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"

resolvers ++= Seq(ai2PublicReleases, cogcompReleases)

unmanagedBase := baseDirectory.value / "lib"

includeFilter in unmanagedJars := "*.jar"

javaOptions ++= List("-Djava.library.path=lib", "-Xmx10g")

fork := true

