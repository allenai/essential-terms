import sbt.Keys._
import sbt._

organization := "org.allenai.ari"
name := "essential-terms"
version := "1.0"
description := "A pre-processing tool to identify important terms in questions"
scalaVersion := "2.11.5"

// Excludes the weka-stable from a dependency, since we use weka-dev.
def excludeWekaStable(module: ModuleID): ModuleID = {
  module.exclude("nz.ac.waikato.cms.weka", "weka-stable")
}

// cogcomp tools
val illinoisEdison = excludeWekaStable("edu.illinois.cs.cogcomp" % "illinois-edison" % "3.0.47")
val illinoisNlpPipeline = excludeWekaStable(
  "edu.illinois.cs.cogcomp" % "illinois-nlp-pipeline" % "0.1.21"
)
val illinoisLBJava = excludeWekaStable("edu.illinois.cs.cogcomp" % "LBJava" % "1.2.20")
val illinoisSaul = excludeWekaStable("edu.illinois.cs.cogcomp" %% "saul" % "0.4")

val wispPlotting = "com.quantifind" %% "wisp" % "0.0.4"
val redisClient = "net.debasishg" %% "redisclient" % "3.0"

// specific versions needed for illinoisNlpPipeline
val stanfordCorenlp331 = "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1"
val stanfordModels331 = "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models"

val word2VecJava = "com.medallia.word2vec" % "Word2VecJava" % "0.10.3"

val allenAiCommonVersion = "1.4.6"
def allenAiCommonModule(name: String) = "org.allenai.common" %% s"common-$name" % allenAiCommonVersion

val wumpusClient = "org.allenai.wumpus.client" % "wumpus-client-assembly" % "1.1.2"

def textualEntailment(component: String) = "org.allenai.textual-entailment" %% component % "1.0.5"

def nlpstack(component: String) = ("org.allenai.nlpstack" %% s"nlpstack-$component" % "1.6")
  .exclude("commons-logging", "commons-logging")

val sprayVersion = "1.3.3"
def sprayModule(id: String): ModuleID = "io.spray" %% s"spray-$id" % sprayVersion
val sprayCaching = sprayModule("caching")
val sprayClient = sprayModule("client")

libraryDependencies ++= Seq(
  allenAiCommonModule("guice") exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiCommonModule("core") exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiCommonModule("testkit") exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiCommonModule("cache") exclude("edu.stanford.nlp", "stanford-corenlp"),
  wumpusClient exclude("edu.stanford.nlp", "stanford-corenlp"),
  illinoisEdison,
  illinoisNlpPipeline,
  illinoisSaul,
  illinoisLBJava,
  redisClient,
  stanfordCorenlp331,  // different version than included in allenAiCommon
  stanfordModels331,  // different version than included in allenAiCommon
  word2VecJava exclude("edu.stanford.nlp", "stanford-corenlp"),
  wispPlotting,
  "org.allenai" %% "datastore" % "1.0.0",
  nlpstack("tokenize") exclude("edu.stanford.nlp", "stanford-corenlp"),
  nlpstack("lemmatize") exclude("edu.stanford.nlp", "stanford-corenlp"),
  textualEntailment("interface") exclude("edu.stanford.nlp", "stanford-corenlp"),
  textualEntailment("service") exclude("edu.stanford.nlp", "stanford-corenlp"),
  sprayCaching,
  sprayClient
)

dependencyOverrides += allenAiCommonModule("core")

val ai2PublicReleases = Resolver.bintrayRepo("allenai", "maven")
val cogcompReleases = "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"

resolvers ++= Seq(ai2PublicReleases, cogcompReleases)

javaOptions ++= List("-Djava.library.path=lib", "-Xmx10g")
fork := true

lazy val keyFile = new java.io.File(Path.userHome.absolutePath + "/.ssh/id_rsa")
publishTo := Some(
  Resolver.ssh(
    "CogcompSoftwareRepo", "bilbo.cs.illinois.edu",
    "/mounts/bilbo/disks/0/www/cogcomp/html/m2repo/"
  ) as ("khashab2", keyFile)
)