package org.allenai.ari.solvers.common

import com.typesafe.config.Config
import org.allenai.ari.solvers.common.entailers.{Entailer, LocalEntailer, RemoteEntailer}
import org.allenai.common.Config._
import org.allenai.common.Resource
import org.allenai.common.guice.ConfigModule
import org.allenai.datastore.Datastore
import org.allenai.wumpus.client.WumpusModule

import scala.io.Source

/** Module installing a bindings for KeywordTokenizer and EntailmentService.  This requires a
  * binding for ActorSystem if `bindWumpus` is true.
  *
  * See resources/org/allenai/solvers/common/common.conf for config values.
  * @param bindWumpus if true, install the WumpusModule using the config values in this module
  */
class SolversCommonModule(
    config: Config,
    bindWumpus: Boolean = false
) extends ConfigModule(config) {

  override def configName: Option[String] = Some("common.conf")

  override def configureWithConfig(fullConfig: Config): Unit = {
    if (bindWumpus) {
      install(new WumpusModule(fullConfig[Config]("wumpus-overrides")))
    }

    val stoplistConfig: Config = fullConfig[Config]("stoplist")
    val stoplistLocationOption = stoplistConfig.get[String]("selected") match {
      case Some(key @ ("aristo" | "smart")) => Some(stoplistConfig[Config](key))
      case Some(key) => {
        addError(s"""bad configuration value "$key" for KeywordTokenizer stoplist""")
        None
      }
      case None => {
        addError("missing configuration key for KeywordTokenizer stoplist")
        None
      }
    }
    stoplistLocationOption foreach { location: Config =>
      val stoplist = SolversCommonModule.loadStopwords(
        location[String]("group"),
        location[String]("name"),
        location[Int]("version")
      )
      bind[Set[String]].annotatedWithName("tokenizerStopwords").toInstance(stoplist)
    }
    val substitutions: Map[String, String] = if (fullConfig[Boolean]("useSubstitutions")) {
      KeywordTokenizer.defaultSubstitutions
    } else {
      Map.empty
    }
    bind[Map[String, String]].annotatedWithName("tokenizerSubstitutions").toInstance(substitutions)

    val useLocalEntailment = fullConfig[Boolean]("entailment.useLocal")

    // Bind the appropriate entailer.
    if (useLocalEntailment) {
      bind[Entailer].to[LocalEntailer]
    } else {
      bind[Entailer].to[RemoteEntailer]
    }
  }
}
object SolversCommonModule {
  /** Loads a stopwords set from the given datastore resource. This assumes one stopword per line in
    * the file.
    */
  def loadStopwords(group: String, name: String, version: Int): Set[String] = {
    Resource.using(Source.fromFile(Datastore.filePath(group, name, version).toFile)) { source =>
      source.getLines().toSet
    }
  }
}
