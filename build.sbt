name := "termselector"

description := "A pre-processing tool to identify important terms in questions"

jvmMemory := "8g"

libraryDependencies ++= Seq(
  allenAiCommon.exclude("edu.stanford.nlp", "stanford-corenlp"),
  allenAiGuice.exclude("edu.stanford.nlp", "stanford-corenlp"),
  aristoCommon,
  aristoDatamodel,
  datastore,
  illinoisEdison,
  illinoisNlpPipeline,
  illinoisSaul,
  illinoisLBJava,
  redisClient,
  rephraseCore,
  stanfordCorenlp331,  // different version than included in allenAiCommon
  stanfordModels331,  // different version than included in allenAiCommon
  word2VecJava.exclude("edu.stanford.nlp", "stanford-corenlp"),
  wumpusClient.exclude("edu.stanford.nlp", "stanford-corenlp"),
  wispPlotting
)

logbackConfFile := baseDirectory.value / "conf" / "logback.xml"
