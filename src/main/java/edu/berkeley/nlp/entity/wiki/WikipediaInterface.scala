package edu.berkeley.nlp.entity.wiki

import java.io.File

import scala.Array.canBuildFrom
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.wiki._

/**
 * Crunches Wikipedia into a smaller database relativized to a particular test set.
 * This should be run as a preprocessing step before either training or predicting
 * with the entity model including Wikipedia links.
 * 
 * To run this on new data, run a command like
 * java -cp /path/to/jar -Xmx8g edu.berkeley.nlp.entity.wiki.WikipediaInterface \
 *  -datasetPaths path/to/test-docs-directory-one-doc-per-file,path/to/additional/docs,... \
 *  -wikipediaDumpPath path/to/enwiki-latest-pages-articles.xml
 *  -outputPath path/to/output-file.ser.gz
 *
 * Required arguments:
 * -datasetPaths: pointer to CoNLL-formatted files whose mentions we should extract
 * Wikipedia information for. Can include multiple directories, comma-delimited.
 * -wikipediaDumpPath: pointer to an XML Wikipedia dump; see
 * http://en.wikipedia.org/wiki/Wikipedia:Database_download#English-language_Wikipedia
 * -outputPath: where we should write the output to
 * 
 * Optional arguments:
 * -mentionType should be "ontonotes" if using raw text, "ace" is only relevant if you
 * have gold mentions
 *
 * -loadParses, -parserModelPath, and -backoffParserModel path are used to control whether
 * we parse (to determine predicate nominals) or not.
 * 
 * -computeLinkDB can be used to turn off computation of which pages link to one another,
 * which can cause the final DB to take a lot of memory
 *
 * -categoryDBInputPath and -categoryDBOutputPath are used to save the categories database;
 * because computing categories requires parsing (in order to determine predicate nominals)
 * 
 * 
 * @author gdurrett
 */
@SerialVersionUID(1L)
class WikipediaInterface(val titleGivenSurfaceDB: WikipediaTitleGivenSurfaceDB,
                         val redirectsDB: WikipediaRedirectsDB,
                         val categoryDB: WikipediaCategoryDB,
                         val linksDB: WikipediaLinkDB,
                         val auxDB: WikipediaAuxDB,
                         val textDB: WikipediaTextDB) extends Serializable {
  
  def getStandardPriorForJointModel(ment: Mention) = {
    val counter = new Counter[String];
    if (ment.mentionType.isClosedClass) {
      counter.incrementCount(ExcludeToken, 2.0);
    } else {
      counter.incrementAll(disambiguateBestGetAllReasonableOptions(ment, ment.headIdx))
      if (counter.isEmpty) {
        counter.incrementCount(NilToken, 2.0);
      }
    }
    counter;
  }
  
  def disambiguate(ment: Mention) = disambiguateBest(ment, ment.headIdx)
  
  def disambiguateBest(ment: Mention, specifiedHeadIdx: Int) = {
    redirectsDB.followRedirect(
      titleGivenSurfaceDB.disambiguateQueries(
        Query.extractQueriesBest(ment).map(_.getFinalQueryStr)));
  }
  
  def disambiguateBestNoDisambig(query: Query) = {
    val queryStr = query.getFinalQueryStr;
    if (titleGivenSurfaceDB.surfaceToTitle.containsKey(queryStr )) {
      val counter = titleGivenSurfaceDB.surfaceToTitle.getCounter(queryStr);
      val redirectedCounts = redirectsDB.followRedirectsCounter(counter);
      auxDB.purgeDisambiguationAll(redirectedCounts);
      if (!redirectedCounts.isEmpty) {
        redirectedCounts.argMax;
      } else {
        NilToken
      }
    } else {
      NilToken;
    }
  }
  
  def disambiguateBestGetAllOptions(ment: Mention, specifiedHeadIdx: Int) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllOptions(
          Query.extractQueriesBest(ment).map(_.getFinalQueryStr))));
  }
  
  def disambiguateBestGetAllOptions(query: Query) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllOptions(
          Seq(query.getFinalQueryStr))));
  }

  def merge[T](a: Counter[T], b: Counter[T]) = {
    for(k <- a.keySet().asScala) {
      b.incrementCount(k, a.getCount(k))
    }
  }

  def disambigRes(query: Query) = {
    val str = query.getFinalQueryStr
    var titles = titleGivenSurfaceDB.disambiguateQueriesGetAllOptions(Seq(str))
    titles.incrementCount(str, 1.0)
    var redirs = redirectsDB.followRedirectsCounter(titles)
    merge(titles, redirs)
    //var aux = auxDB.purgeDisambiguationAll(redirs)
    //merge(redirs, aux)
    //aux
    redirs
  }



  def disambiguateBestGetAllReasonableOptions(ment: Mention, specifiedHeadIdx: Int) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllReasonableOptions(
          Query.extractQueriesBest(ment).map(_.getFinalQueryStr))));
  }
  
  def disambiguateBestGetAllOneBestOptions(ment: Mention, specifiedHeadIdx: Int) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllOneBestOptions(
          Query.extractQueriesBest(ment).map(_.getFinalQueryStr))));
  }
  
  def getCategories(title: String) = categoryDB.getCategories(title);
  def getCategoriesSortedByFrequency(title: String) = categoryDB.getCategoriesSortedByFrequency(title);
  def getTopKCategoriesByFrequency(title: String, k: Int) = categoryDB.getTopKCategoriesByFrequency(title, k);
  def getInfobox(title: String) = categoryDB.getInfobox(title);
  def getInfoboxHead(title: String) = categoryDB.getInfoboxHead(title);
  def getAppositive(title: String) = categoryDB.getAppositive(title);
  
  def printSome() {
    Logger.logss("Title given surface: " + titleGivenSurfaceDB.surfaceToTitle.size() + " surfaces, " +
                 titleGivenSurfaceDB.surfaceToTitle.keySet.asScala.flatMap(key => titleGivenSurfaceDB.surfaceToTitle.getCounter(key).keySet.asScala).size + " titles");
    val surfaceKeys = titleGivenSurfaceDB.surfaceToTitle.keySet().asScala;
    for (key <- surfaceKeys.slice(0, Math.min(surfaceKeys.size, 10))) {
      Logger.logss(key + " -> " + titleGivenSurfaceDB.surfaceToTitle.getCounter(key));
    }
    printSome(redirectsDB.redirects, "redirects");
    Logger.logss("Categories given titles: " + categoryDB.categoryMap.size + " titles, " + categoryDB.categoryMap.map(_._2.size).foldLeft(0)(_ + _) + " category entries");
    for (key <- categoryDB.categoryMap.keySet.slice(0, Math.min(categoryDB.categoryMap.keySet.size, 100))) {
      Logger.logss(key + " -> " + categoryDB.categoryMap(key));
    }
    printSome(categoryDB.infoboxMap, "infoboxMap")
    printSome(categoryDB.appositiveMap, "appositiveMap")
  }
  
  def printSome(map: HashMap[String,String], message: String) {
    Logger.logss(message + ": " + map.size + " keys");
    for (key <- map.keySet.slice(0, Math.min(map.keySet.size, 100))) {
      Logger.logss(key + " -> " + map(key));
    }
  }
}

object WikipediaInterface {
  
  val MaxSnippetChars = 1000;
  
  val datasetPaths = "";
  val wikipediaDumpPath = "";
  val outputPath = "";

  val docSuffix = "auto_conll";
  val mentionType = "ontonotes"; // ace, ontonotes, old
  
  val loadParsers = true;
  val parserModelPath = "models/eng_sm6.gr";
  val backoffParserModelPath = "models/eng_sm1.gr";
  
  val computeLinkDB = true;
  
  val categoryDBInputPath = "";
  val categoryDBOutputPath = "";

  val wikiStandoff = "";
  
  def processWikipedia(wikipediaPath: String, queries: Set[String], parser: CoarseToFineMaxRuleParser, backoffParser: CoarseToFineMaxRuleParser, strUnifier: StringUnifier): WikipediaInterface = {
    val titleGivenSurface = WikipediaTitleGivenSurfaceDB.processWikipedia(wikipediaPath, queries, strUnifier);
    val redirects = WikipediaRedirectsDB.processWikipedia(wikipediaPath, titleGivenSurface, strUnifier);
    val allPageTargetsLc = titleGivenSurface.allPossibleTitlesLowercase.toSet ++ redirects.possibleRedirectTargetsLc;
    val links = if (WikipediaInterface.computeLinkDB) {
      WikipediaLinkDB.processWikipedia(wikipediaPath, allPageTargetsLc, strUnifier);
    } else {
      new WikipediaLinkDB(new Indexer[String], new HashMap[Int,IntArray], new HashMap[Int,IntArray]);
    }
    // TODO:? make cat use the string unifier as well
    val categories = WikipediaCategoryDB.processWikipedia(wikipediaPath, allPageTargetsLc, parser, backoffParser);
    val aux = WikipediaAuxDB.processWikipedia(wikipediaPath, allPageTargetsLc, strUnifier);
    val texts = WikipediaTextDB.processWikipedia(wikipediaPath, allPageTargetsLc, strUnifier);
    val wi = new WikipediaInterface(titleGivenSurface, redirects, categories, links, aux, texts);
    wi.printSome();
    wi;
  }
  
  def processWikipedia(wikipediaPath: String, queries: Set[String], categoryDB: WikipediaCategoryDB, strUnifier: StringUnifier): WikipediaInterface = {
    val titleGivenSurface = WikipediaTitleGivenSurfaceDB.processWikipedia(wikipediaPath, queries, strUnifier);
    val redirects = WikipediaRedirectsDB.processWikipedia(wikipediaPath, titleGivenSurface, strUnifier);
    val allPageTargetsLc = titleGivenSurface.allPossibleTitlesLowercase.toSet ++ redirects.possibleRedirectTargetsLc;
    val links = if (WikipediaInterface.computeLinkDB) {
      WikipediaLinkDB.processWikipedia(wikipediaPath, allPageTargetsLc, strUnifier);
    } else {
      new WikipediaLinkDB(new Indexer[String], new HashMap[Int,IntArray], new HashMap[Int,IntArray]);
    }
    val aux = WikipediaAuxDB.processWikipedia(wikipediaPath, allPageTargetsLc, strUnifier);
    val texts = WikipediaTextDB.processWikipedia(wikipediaPath, allPageTargetsLc, strUnifier);
    val wi = new WikipediaInterface(titleGivenSurface, redirects, categoryDB, links, aux, texts);
    wi.printSome();
    wi;
  }
  
//  def getWikipediaInterface
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(WikipediaInterface.getClass);
    LightRunner.populateScala(WikipediaInterface.getClass, args);
//    WikipediaInterface.populate(args);
    val (parser, backoffParser): (CoarseToFineMaxRuleParser, CoarseToFineMaxRuleParser) = if (WikipediaInterface.loadParsers) {
      Logger.logss("Loading parser");
      val parser = PreprocessingDriver.loadParser(WikipediaInterface.parserModelPath);
      Logger.logss("Loading backoff parser");
      val backoffParser = PreprocessingDriver.loadParser(WikipediaInterface.backoffParserModelPath);
      (parser, backoffParser)
    } else {
      (null, null)
    }
//    val flags = args.slice(3, args.size);
    val mentionPropertyComputer = new MentionPropertyComputer(None);
    val pmAssembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = false);
    val gmAssembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = true);
    val corefDocs = WikipediaInterface.datasetPaths.split(",").flatMap(path_ => {
      var path = path_
      val mentionType = if(path.contains(":")) {
        val s = path.split(":")
        path = s(1)
        s(0)
      } else {
        WikipediaInterface.mentionType
      }
      Logger.logss("Loading documents "+mentionType+" "+path)
      if (mentionType == "old") {
        // Wikification dataset: use only auto_conll and pred mentions
        ConllDocReader.loadRawConllDocsWithSuffix(path, -1, "", Language.ENGLISH).map(doc => pmAssembler.createCorefDoc(doc, mentionPropertyComputer));
      } else if (mentionType == "ace") {
        // ACE: Use gold mentions here
        ConllDocReader.loadRawConllDocsWithSuffix(path, -1, "", Language.ENGLISH).map(doc => gmAssembler.createCorefDoc(doc, mentionPropertyComputer));
      } else if (mentionType == "ontonotes") {
        // OntoNotes: use only auto_conll and pred mentions
        ConllDocReader.loadRawConllDocsWithSuffix(path, -1, docSuffix, Language.ENGLISH).map(doc => pmAssembler.createCorefDoc(doc, mentionPropertyComputer));
      } else if (mentionType == "wikiser") {
        WikiDocReader.loadRawWikiDocs(path, -1, docSuffix, Language.ENGLISH).map(doc => {
          try {
            gmAssembler.createCorefDoc(doc, mentionPropertyComputer)
          } catch {
            case e : Exception => {
              // there are currently about 30 documents that are having an issue with their references
              println("FAIL DOCUMENT: "+doc.docID)
              null
            }
          }
        })
      } else {
        throw new RuntimeException("Unrecognized mention type: " + WikipediaInterface.mentionType);
      }
    }).filter(_!=null);
//    val queries = corefDocs.flatMap(_.predMentions.filter(!_.mentionType.isClosedClass)).flatMap(ment => WikipediaTitleGivenSurfaceDB.extractQueries(ment, ment.headIdx)).toSet;

    // MFL TODO: this is the queries that will have to be rewritten to support the wiki documents.
    var queries = corefDocs.flatMap(_.predMentions/*.filter(!_.mentionType.isClosedClass)*/)
      .flatMap(ment => Query.extractQueriesBest(ment).map(_.getFinalQueryStr))
      .toSet;
    // some of the gold titles in the older dataset link to current redirect pages
    // so we are loading them here so we can normalize the redirects when performing training/testing
    val golds : Set[String] = if(!wikiStandoff.isEmpty) {
      WikiAnnotReaderWriter.readStandoffAnnotsAsCorpusAnnots(wikiStandoff).flatMap(d => {
        d._2.flatMap(v => {
          v._2.flatMap(_.label).map(_.replace("_"," "))
        })
      }).toSet
    } else {
      Set[String]()
    }

    IntArray.prefixDir = new File(WikipediaInterface.outputPath).getParent

    queries = queries ++ golds
    Logger.logss("Extracted " + queries.size + " queries from " + corefDocs.size + " documents");
    val strUnifier = new StringUnifier
    queries = queries.map(strUnifier(_))
    val interface = if (WikipediaInterface.categoryDBInputPath != "") {
      val categoryDB = GUtil.load(WikipediaInterface.categoryDBInputPath).asInstanceOf[WikipediaCategoryDB];
      processWikipedia(WikipediaInterface.wikipediaDumpPath, queries, categoryDB, strUnifier);
    } else {
      processWikipedia(WikipediaInterface.wikipediaDumpPath, queries, parser, backoffParser, strUnifier);
    } 
    GUtil.save(interface, WikipediaInterface.outputPath);
    if (WikipediaInterface.categoryDBOutputPath != "") {
      GUtil.save(interface.categoryDB, WikipediaInterface.categoryDBOutputPath);
    }
    LightRunner.finalizeOutput();
  }
}
