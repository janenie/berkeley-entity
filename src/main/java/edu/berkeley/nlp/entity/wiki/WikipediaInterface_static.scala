package edu.berkeley.nlp.entity.wiki

import scala.Array.canBuildFrom
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.{WikiDocReader, ConllDocReader, GUtil}
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
class WikipediaInterface_static(val titleGivenSurfaceDB: WikipediaTitleGivenSurfaceDB,
                         val redirectsDB: WikipediaRedirectsDB,
                         val categoryDB: WikipediaCategoryDB,
                         val linksDB: WikipediaLinkDB,
                         val auxDB: WikipediaAuxDB) extends Serializable with WikipediaInterface2 {
  
  override def getStandardPriorForJointModel(ment: Mention) = {
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
  
  // def disambiguate(ment: Mention) = disambiguateBest(ment, ment.headIdx)
  
  override def disambiguateBest(ment: Mention, specifiedHeadIdx: Int) = {
    redirectsDB.followRedirect(titleGivenSurfaceDB.disambiguateQueries(Query.extractQueriesBest(ment).map(_.getFinalQueryStr)));
  }
  
  override def disambiguateBestNoDisambig(query: Query) = {
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
  
  override def disambiguateBestGetAllOptions(ment: Mention, specifiedHeadIdx: Int) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllOptions(
          Query.extractQueriesBest(ment).map(_.getFinalQueryStr))));
  }
  
  override  def disambiguateBestGetAllOptions(query: Query) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllOptions(
          Seq(query.getFinalQueryStr))));
  }

  override def disambigRes(query: Query) = {
    titleGivenSurfaceDB.disambiguateQueriesGetAllOptions(Seq(query.getFinalQueryStr))
  }

  override def disambiguateBestGetAllReasonableOptions(ment: Mention, specifiedHeadIdx: Int) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllReasonableOptions(
          Query.extractQueriesBest(ment).map(_.getFinalQueryStr))));
  }
  
  override def disambiguateBestGetAllOneBestOptions(ment: Mention, specifiedHeadIdx: Int) = {
    auxDB.purgeDisambiguationAll(
      redirectsDB.followRedirectsCounter(
        titleGivenSurfaceDB.disambiguateQueriesGetAllOneBestOptions(
          Query.extractQueriesBest(ment).map(_.getFinalQueryStr))));
  }
  
  override def getCategories(title: String) = categoryDB.getCategories(title);
  override def getCategoriesSortedByFrequency(title: String) = categoryDB.getCategoriesSortedByFrequency(title);
  override def getTopKCategoriesByFrequency(title: String, k: Int) = categoryDB.getTopKCategoriesByFrequency(title, k);
  override def getInfobox(title: String) = categoryDB.getInfobox(title);
  override def getInfoboxHead(title: String) = categoryDB.getInfoboxHead(title);
  override def getAppositive(title: String) = categoryDB.getAppositive(title);
  
  override def printSome() {
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

  def getStandardPriorForJointModel(ment: Any): Counter[String] = ???
}

object WikipediaInterface_static {
  
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
  
  def processWikipedia(wikipediaPath: String, queries: Set[String], parser: CoarseToFineMaxRuleParser, backoffParser: CoarseToFineMaxRuleParser): WikipediaInterface_static = {
    val titleGivenSurface = WikipediaTitleGivenSurfaceDB.processWikipedia(wikipediaPath, queries);
    val redirects = WikipediaRedirectsDB.processWikipedia(wikipediaPath, titleGivenSurface);
    val allPageTargetsLc = titleGivenSurface.allPossibleTitlesLowercase.toSet ++ redirects.possibleRedirectTargetsLc;
    val links = if (WikipediaInterface_static.computeLinkDB) {
      WikipediaLinkDB.processWikipedia(wikipediaPath, allPageTargetsLc);
    } else {
      new WikipediaLinkDB(new Indexer[String], new HashMap[String,Array[Int]], new HashMap[String,Array[Int]]);
    }
    val categories = WikipediaCategoryDB.processWikipedia(wikipediaPath, allPageTargetsLc, parser, backoffParser);
    val aux = WikipediaAuxDB.processWikipedia(wikipediaPath, allPageTargetsLc);
    val wi = new WikipediaInterface_static(titleGivenSurface, redirects, categories, links, aux);
    wi.printSome();
    wi;
  }
  
  def processWikipedia(wikipediaPath: String, queries: Set[String], categoryDB: WikipediaCategoryDB): WikipediaInterface_static = {
    val titleGivenSurface = WikipediaTitleGivenSurfaceDB.processWikipedia(wikipediaPath, queries);
    val redirects = WikipediaRedirectsDB.processWikipedia(wikipediaPath, titleGivenSurface);
    val allPageTargetsLc = titleGivenSurface.allPossibleTitlesLowercase.toSet ++ redirects.possibleRedirectTargetsLc;
    val links = if (WikipediaInterface_static.computeLinkDB) {
      WikipediaLinkDB.processWikipedia(wikipediaPath, allPageTargetsLc);
    } else {
      new WikipediaLinkDB(new Indexer[String], new HashMap[String,Array[Int]], new HashMap[String,Array[Int]]);
    }
    val aux = WikipediaAuxDB.processWikipedia(wikipediaPath, allPageTargetsLc);
    val wi = new WikipediaInterface_static(titleGivenSurface, redirects, categoryDB, links, aux);
    wi.printSome();
    wi;
  }
  
//  def getWikipediaInterface
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(WikipediaInterface_static.getClass);
    LightRunner.populateScala(WikipediaInterface_static.getClass, args);
//    WikipediaInterface.populate(args);
    val (parser, backoffParser): (CoarseToFineMaxRuleParser, CoarseToFineMaxRuleParser) = if (WikipediaInterface_static.loadParsers) {
      Logger.logss("Loading parser");
      val parser = PreprocessingDriver.loadParser(WikipediaInterface_static.parserModelPath);
      Logger.logss("Loading backoff parser");
      val backoffParser = PreprocessingDriver.loadParser(WikipediaInterface_static.backoffParserModelPath);
      (parser, backoffParser)
    } else {
      (null, null)
    }
//    val flags = args.slice(3, args.size);
    val mentionPropertyComputer = new MentionPropertyComputer(None);
    val pmAssembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = false);
    val gmAssembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = true);
    val corefDocs = WikipediaInterface_static.datasetPaths.split(",").flatMap(path_ => {
      var path = path_
      val mentionType = if(path.contains(":")) {
        val s = path.split(":")
        path = s(1)
        s(0)
      } else {
        WikipediaInterface_static.mentionType
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
        throw new RuntimeException("Unrecognized mention type: " + WikipediaInterface_static.mentionType);
      }
    }).filter(_!=null);
//    val queries = corefDocs.flatMap(_.predMentions.filter(!_.mentionType.isClosedClass)).flatMap(ment => WikipediaTitleGivenSurfaceDB.extractQueries(ment, ment.headIdx)).toSet;

    // MFL TODO: this is the queries that will have to be rewritten to support the wiki documents.
    val queries = corefDocs.flatMap(_.predMentions.filter(!_.mentionType.isClosedClass)).flatMap(ment => Query.extractQueriesBest(ment).map(_.getFinalQueryStr)).toSet;
    Logger.logss("Extracted " + queries.size + " queries from " + corefDocs.size + " documents");
    val interface = if (WikipediaInterface_static.categoryDBInputPath != "") {
      val categoryDB = GUtil.load(WikipediaInterface_static.categoryDBInputPath).asInstanceOf[WikipediaCategoryDB];
      processWikipedia(WikipediaInterface_static.wikipediaDumpPath, queries, categoryDB);
    } else {
      processWikipedia(WikipediaInterface_static.wikipediaDumpPath, queries, parser, backoffParser);
    } 
    GUtil.save(interface, WikipediaInterface_static.outputPath);
    if (WikipediaInterface_static.categoryDBOutputPath != "") {
      GUtil.save(interface.categoryDB, WikipediaInterface_static.categoryDBOutputPath);
    }
    LightRunner.finalizeOutput();
  }
}
