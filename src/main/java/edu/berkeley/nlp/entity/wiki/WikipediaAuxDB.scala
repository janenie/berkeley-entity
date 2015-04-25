package edu.berkeley.nlp.entity.wiki

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.preprocess.Reprocessor
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
import edu.berkeley.nlp.entity.preprocess.SentenceSplitter
import edu.berkeley.nlp.entity.{StringUnifier, DepConstTree}
import scala.collection.mutable.HashSet

@SerialVersionUID(1L)
class WikipediaAuxDB(val disambiguationSet: HashSet[String]) extends Serializable {
  def isDisambiguation(pageTitle: String) = disambiguationSet.contains(pageTitle);
  
  def purgeDisambiguationAll(counter: Counter[String]) = {
    for (key <- counter.keySet.asScala.toSeq) {
      if (isDisambiguation(key)) {
//        Logger.logss("Purging " + key);
        counter.removeKey(key);
      }
    }
    counter;
  }
}

object WikipediaAuxDB {
  
  def processWikipedia(wikipediaPath: String,
                       pageTitleSetLc: Set[String],
                       strUnifier: StringUnifier): WikipediaAuxDB = {
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle = "";
    var doneWithThisPage = false;
    var isInText = false;
    val disambiguationSet = new HashSet[String]
    // Extract first line that's not in brackets
    while (lines.hasNext) {
      val line = lines.next;
      if (line.length > 8 && doneWithThisPage) {
        // Do nothing
      } else {
        if (line.contains("<page>")) {
          doneWithThisPage = false;
        } else if (line.contains("<title>")) {
          currentPageTitle = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>"));
          if (!pageTitleSetLc.contains(currentPageTitle.toLowerCase)) {
            doneWithThisPage = true;
          }
        }
        if (!doneWithThisPage && (line.startsWith("{{disambiguation}}") || line.startsWith("{{disambiguation|") ||
                                  line.startsWith("{{disambig}}") || line.startsWith("{{hndis"))) {
          disambiguationSet += strUnifier(currentPageTitle);
          doneWithThisPage = true;
        }
      }
    }
    new WikipediaAuxDB(disambiguationSet);
  }
}
