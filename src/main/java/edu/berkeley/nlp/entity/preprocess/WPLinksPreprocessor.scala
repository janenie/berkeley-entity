package edu.berkeley.nlp.entity.preprocess

import java.io.{BufferedReader, File, FileReader}

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.lang.ModCollinsHeadFinder
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.{Chunk, DepConstTree, WikiDoc}
import edu.berkeley.nlp.futile.fig.basic.{IOUtils, Indexer}
import edu.berkeley.nlp.futile.util.Logger

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Created by matthewfl
 */
object WPLinksPreprocessor {

  val headFinder = new ModCollinsHeadFinder()

  def processesDocs (inputFile : String, outputDir : String,
                     //docReader : WikiDocReader,
                     splitter : SentenceSplitter,
                     parser : CoarseToFineMaxRuleParser,
                     backoffParser : CoarseToFineMaxRuleParser,
                     nerSystem : NerSystemLabeled) = {
    // the data comes in a single input file
    val f = new BufferedReader(new FileReader(new File(inputFile)))
    val documents = new mutable.HashMap[String, List[String]]()
    var currentDocument:  mutable.MutableList[String] = null
    var line: String = f.readLine()
    var docName: String = null
    while(line != null) {
//      if(line.startsWith("#end")) {
//        currentDocument += line
//        documents.put(docName, currentDocument.toList)
//        docName = null
//        currentDocument = null
//      } else
      if(line.startsWith("-DOCSTART-")) {
        if(docName != null) {
          documents.put(docName, currentDocument.toList)
        }
        currentDocument = new mutable.MutableList[String]()
        currentDocument += line
        docName = ".*\\((.*)\\).*".r.findFirstMatchIn(line).get.group(1)
      } else {
        currentDocument += line
      }
      line = f.readLine()
    }
    if(docName != null) {
      documents.put(docName, currentDocument.toList)
    }


    val wpLinkDocs = documents.par.map(doc => {
      try {
        process(doc._1, doc._2, outputDir + doc._1, splitter, parser.newInstance(), backoffParser.newInstance(), nerSystem)
      } catch {
        case e: Exception => {
          Logger.logss("failed document: "+doc._1)
          System.err.print(e.toString)
          e.printStackTrace(System.err)
          null
        }
      }
    }).filter(_ != null).toList
  }

  def process(documentName: String, documentContent: List[String], outputFile: String,
              //docReader : WikiDocReader,
              splitter : SentenceSplitter,
              parser : CoarseToFineMaxRuleParser,
              backoffParser : CoarseToFineMaxRuleParser,
              nerSystem : NerSystemLabeled) = {

    val cdoc = WPLinksToWikidoc(documentName, documentContent, splitter, parser, backoffParser, nerSystem)

    val lines = WPLinksToConllLines(cdoc)
    val wlines = WPLinksToWikiLines(cdoc)

    writeLines(documentName, lines, outputFile)
    writeLines(documentName, wlines, outputFile.replace("raw", "wiki"))

  }

   def writeLines(docID : String, lines : Seq[Seq[String]], outputFile : String) = {
     var writer = IOUtils.openOutHard(outputFile)
     writer.println("#begin document (" + docID + "); part 000")
     lines.foreach(l => {
       l.foreach(writer.println(_))
       writer.println
     })
     //  writer.println("#end") // wtf, wiki without the #end
     writer.close()
  }




  def WPLinksToWikidoc(documentName: String, content: List[String],
                       //docReader : WikiDocReader,
                       splitter : SentenceSplitter,
                       parser : CoarseToFineMaxRuleParser,
                       backoffParser : CoarseToFineMaxRuleParser,
                       nerSystem : NerSystemLabeled): WikiDoc = {
    Logger.logss("starting processing of: "+documentName)
    // document id \t 0 \t position in sentence \t word \t indication if a reference \t link reference to
    val csplit = content.map(_.split("\t"))

    val sentences = ListBuffer[Seq[String]]()
    //var sentencePlace = 0
    val references = ListBuffer[Seq[Chunk[String]]]()
    var sentenceWords = ListBuffer[String]()
    var sentenceReferences = ListBuffer[Chunk[String]]()
    var currentReferenceStart = -1
    var currentReference: String = null
    def saveReference(end: Int): Unit = {
      if(currentReference != null) {
        val name = currentReference.split("/").last // this are full http links to wikipedia, only want the page title
        sentenceReferences += new Chunk[String](currentReferenceStart, end, name)
        currentReference = null
        currentReferenceStart = -1
      }
    }
    for(i <- 1 until csplit.size - 1) { // ignore the begin and end documents
      if(false && csplit(i).length == 1 && csplit(i)(0).isEmpty) {
        // this is the start of a new sentence
        saveReference(sentenceWords.length)
        sentences += sentenceWords.toSeq
        references += sentenceReferences.toSeq
        sentenceWords = ListBuffer[String]()
        sentenceReferences = ListBuffer[Chunk[String]]()
      } else {
        sentenceWords += csplit(i)(0)
        if(csplit(i).length > 3) {
          if(currentReference == csplit(i)(3)) {
            // this is still the same
          } else {
            saveReference(sentenceWords.length - 1)
            currentReference = csplit(i)(3)
            currentReferenceStart = sentenceWords.length - 1
          }
        } else if(currentReference != null) {
          saveReference(sentenceWords.length - 1)
        }
      }
    }
    if(sentenceWords.length > 0) {
      sentences += sentenceWords.toSeq
      references += sentenceReferences.toSeq
    }

    val parses = sentences.map(t => {
      Reprocessor.convertToFutileTree(PreprocessingDriver.parse(parser, backoffParser, t.toList.asJava))
    })

    val pos = parses.map(t => { new ArrayBuffer[String] ++ t.getPreTerminalYield.asScala })

    val trees = for(i <- 0 until parses.length) yield {
      val childParentMap = DepConstTree.extractDependencyStructure(parses(i), headFinder)
      new DepConstTree(parses(i), pos(i), sentences(i), childParentMap)
    }

    val empty = sentences.map(l => (0 until l.length).map(a=>"-")).toSeq

    val indexer = new Indexer[String]()

    val wikidoc = new WikiDoc(
      docID=documentName,
      docPartNo = 123,  // idk
      words=sentences.toSeq,
      pos=pos,
      trees=trees,
      nerChunks = sentences.map(a=>Seq()), // TODO:?
      corefChunks = references.map(r => r.map(c => new Chunk(c.start, c.end, indexer.getIndex(c.label)))),
      speakers = empty,
      wikiRefChunks = references
    )

    Logger.logss("finished processing: "+documentName)

    wikidoc
  }

  def WPLinksToConllLines(doc: WikiDoc) = {
    val ret = new ListBuffer[Seq[String]]()
    for(i <- 0 until doc.numSents) {
      val parseBits = PreprocessingDriver.computeParseBits(Reprocessor.convertFromFutileTree(doc.trees(i).constTree))
      val corefBits = computeBits(doc.corefChunks(i), doc.words(i).size)
      var lines = new ListBuffer[String]()
      for(j <- 0 until doc.words(i).size) {
        lines.append(doc.docID + "\t" +
          doc.docPartNo + "\t" +
          j + "\t" +
          doc.words(i)(j) + "\t" +
          doc.pos(i)(j) + "\t" +
          parseBits(j) + "\t" +
          "\t-\t-\t-\t" +
          "-\t" + // speakers
          "*\t" + // nerbit
          corefBits(j) + "\t" // coref bits
        )
      }
      ret.append(lines.toSeq)
    }
    ret.toSeq
  }

  def computeBits[T](items: Seq[Chunk[T]], len: Int): Array[String] = {
    var ret = Array.fill(len)(List[String]())
    items.foreach(c => {
      if(c.start == c.end -1) {
        ret(c.start) = ret(c.start) :+ ("(" + c.label + ")")
      } else {
        ret(c.start) = ret(c.start) :+ ("(" + c.label)
        ret(c.end - 1) = ret(c.end - 1) :+ (c.label + ")")
      }
    })
    ret.map(i => {if(i.isEmpty) "-" else i.reduce(_+"|"+_)})
  }

  def WPLinksToWikiLines(wdoc: WikiDoc): Seq[Seq[String]] = {
    for (sentIdx <- 0 until wdoc.words.size) yield {
      for (tokenIdx <- 0 until wdoc.words(sentIdx).size) yield {
        val chunksStartingHere = wdoc.wikiRefChunks(sentIdx).filter(chunk => chunk.start == tokenIdx).sortBy(- _.end);
        val numChunksEndingHere = wdoc.wikiRefChunks(sentIdx).filter(chunk => chunk.end - 1 == tokenIdx).size;
        var str = if(chunksStartingHere.isEmpty) "" else {
          chunksStartingHere.map("("+_.label.replace("(", "-LRB-").replace(")", "-RRB-").replace("*", "-STAR-")).reduce(_+"|"+_)
        }
        str += "*";
        str += ")" * numChunksEndingHere
        str;
      }
    }
  }

}
