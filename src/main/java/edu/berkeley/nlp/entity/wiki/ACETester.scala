package edu.berkeley.nlp.entity.wiki

import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.Chunk

object ACETester {
  
  // Command line options
  val dataPath = "data/ace05/ace05-all-conll"
  val wikiDBPath = "models/wiki-db-ace.ser.gz"
  val wikiPath = "data/ace05/ace05-all-conll-wiki"
  val useFancyQueryChooser = false;
    
  def main(args: Array[String]) {
    LightRunner.initializeOutput(ACETester.getClass());
    LightRunner.populateScala(ACETester.getClass(), args)
    val docs = ConllDocReader.loadRawConllDocsWithSuffix(dataPath, -1, "", Language.ENGLISH);
//    val goldWikification = GUtil.load(wikiAnnotsPath).asInstanceOf[CorpusWikiAnnots];
    
    val goldWikification = WikiAnnotReaderWriter.readStandoffAnnotsAsCorpusAnnots(wikiPath)
    
    // Detect mentions, which depend on the NER coarse pass
    val assembler = CorefDocAssembler(Language.ENGLISH, true);
    val corefDocs = docs.map(doc => assembler.createCorefDoc(doc, new MentionPropertyComputer(None)));

//     This does super, super well but is probably cheating
//    val wikiDB = GUtil.load(wikiDBPath).asInstanceOf[WikipediaInterface];
//    val trainDataPath = "data/ace05/train";
//    val trainDocs = ConllDocReader.loadRawConllDocsWithSuffix(trainDataPath, -1, "", Language.ENGLISH);
//    val trainCorefDocs = trainDocs.map(doc => assembler.createCorefDoc(doc, new MentionPropertyComputer(None)));
//    val wikifier = new BasicWikifier(wikiDB, Some(trainCorefDocs), Some(goldWikification));
    
    val queryChooser = if (useFancyQueryChooser) {
      GUtil.load("models/querychooser.ser.gz").asInstanceOf[QueryChooser]
    } else {
      val fi = new Indexer[String];
      fi.getIndex("FirstNonempty=true");
      fi.getIndex("FirstNonempty=false");
      new QueryChooser(fi, Array(1F, -1F))
    }
    
    
    val wikiDB = GUtil.load(wikiDBPath).asInstanceOf[WikipediaInterface];
    val wikifier = new BasicWikifier(wikiDB, Some(queryChooser));
    
//    val wikiDB = GUtil.load(wikiDBPath).asInstanceOf[WikipediaInterface];
//    val aceHeads = ACEMunger.mungeACEToGetHeads("data/ace05/ace05-all-copy");
//    val wikifier = new BasicWikifier(wikiDB, None, None, Some(aceHeads));
    
//    val wikifier: Wikifier = FahrniWikifier.readFahrniWikifier("data/wikipedia/lex.anchor.lowAmbiguity-resolved",
//                                                               "data/wikipedia/simTerms");
    
    var recalled = 0;
    for (corefDoc <- corefDocs) {
      val docName = corefDoc.rawDoc.docID
      for (i <- 0 until corefDoc.predMentions.size) {
        val ment = corefDoc.predMentions(i);
        val goldLabel = getGoldWikification(goldWikification(docName), ment)
        if (goldLabel.size >= 1 && goldLabel(0) != NilToken) {
          wikifier.oracleWikify(docName, ment, goldLabel);
          val myTitles = wikifier.wikifyGetTitleSet(docName, ment);
          if (containsCorrect(goldLabel, myTitles)) {
            recalled += 1;
          }
          wikifier
        } else if (goldLabel.size == 1 && goldLabel(0) == NilToken) {
          wikifier.oracleWikifyNil(docName, ment);
        }
      }
    }
    Logger.logss("Recalled: " + recalled);
    wikifier.printDiagnostics();
    LightRunner.finalizeOutput();
  }
}
