package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.{StringUnifier, IntArray}
import edu.berkeley.nlp.futile.fig.basic.{IOUtils, Indexer}
import edu.berkeley.nlp.futile.util.Counter

import scala.collection.JavaConversions._


import scala.StringBuilder
import scala.collection.mutable



/**
 * Created by matthewfl
 *
 * Provide bow counts for documents so we can compute the similarity between two documents
 */
@SerialVersionUID(1L)
class WikipediaTextDB (val indexer: Indexer[String],
                       val words: mutable.HashMap[String, IntArray],
                       val context: mutable.HashMap[String, IntArray],
                       val wordDocCnts: IntArray) extends Serializable {

  def getDocument(title: String) = words.getOrElse(title, IntArray.empty)

  def getContext(title: String) = context.getOrElse(title, IntArray.empty)

  def compareVectors(a: IntArray, b: IntArray) = {
    var ai = 0
    var bi = 0
    var simcnt = 0
    while(ai < a.size && bi < b.size) {
      if(a(ai) == b(bi)) {
        simcnt += 1
        ai += 1
        bi += 1
      } else if(a(ai) > b(bi)) {
        bi += 1
      } else {
        ai += 1
      }
    }
    simcnt
  }

  def compareTitles(atitle: String, btitle: String) = compareVectors(getDocument(atitle), getDocument(btitle))

  def makeVector(document: Seq[Seq[String]]): IntArray = {
    document.flatMap(_.map(v => indexer.indexOf(v.toLowerCase))).toSet.filter(_ != -1).toArray.sorted
  }

  def makeContextVector(vector: IntArray): IntArray = {
    // 300 might be more then the number of words in the document
    // so this might not change anything....
    // maybe shrink this to the top half of words so it is meaningful idk

    // we are going to shrink the size of these even more to attempt to
    // reduce memory usage
    if(vector.size < 300)
      vector
    else
      vector.toArray.sortBy(wordDocCnts(_)).slice(0, 100).sorted
  }

  def compareDocument(doc: IntArray, title: String) = compareVectors(doc, getDocument(title))

  def compareDocumentC(doc: IntArray, title: String) = {
    val tdoc = getDocument(title)
    compareVectors(doc, tdoc).asInstanceOf[Double] / (doc.size * tdoc.size + 1)
  }

  def compareContext(doc: IntArray, title: String) = compareVectors(doc, getContext(title))

  def compareContextC(doc: IntArray, title: String) = {
    val tdoc = getContext(title)
    compareVectors(doc, tdoc).asInstanceOf[Double] / (doc.size * tdoc.size + 1)
  }

}

object WikipediaTextDB {
  def processWikipedia(wikipediaPath:String, querySet: Set[String], strUnifier: StringUnifier) : WikipediaTextDB = {
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle: String = null
    val indexer = new Indexer[String]
    val totalWordCounts = new Counter[Int]
    //var currentWordCounts = new mutable.HashMap[Int,Int]()
    var currentWordCounts = new mutable.HashSet[Int]()
    val documentResults = new mutable.HashMap[String,IntArray]()
    //val documentResultsCount = new mutable.HashMap[String,Array[Int]]()
    var lineIdx = 0
    var numPagesSeen = 0
    var doneWithThisPage = false

    while(lines.hasNext) {
      val line = lines.next
      if (lineIdx % 100000 == 0) {
        println("Line: " + lineIdx + ", processed " + numPagesSeen + " pages");
      }
      lineIdx += 1;
      if (line.length > 8 && doneWithThisPage) {
        // Do nothing
      } else {
        if(line.contains("<page>")) {
          doneWithThisPage = false
          numPagesSeen += 1
        } else if (line.contains("<title>")) {
          // 7 = "<title>".length()
          val newPageTitle = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>"));
          if (!querySet.contains(newPageTitle.toLowerCase)) {
            doneWithThisPage = true;
          } else {
            if(currentPageTitle != null) {
              //val itms = currentWordCounts.map(v => v).toList.sortBy(_._1)
              //documentResults += (currentPageTitle -> itms.map(_._1).toArray)
              //documentResultsCount += (currentPageTitle -> itms.map(_._2).toArray)
              documentResults += (strUnifier(currentPageTitle) -> IntArray.makeDiskBacked(currentWordCounts.toArray.sorted))
            }
            //currentWordCounts = new mutable.HashMap[Int,Int]()
            currentWordCounts = new mutable.HashSet[Int]()
            currentPageTitle = newPageTitle
          }
        } else if(line.contains("<text")) {
          val textStart = line.indexOf(">") + 1
          val document = new StringBuilder()
          var textEnd = line.indexOf("</text>")
          if(textEnd != -1) {
            document.append(line.substring(textStart, textEnd))
          } else {
            var curLine = line.substring(textStart)
            while(textEnd == -1) {
              document.append(curLine)
              curLine = lines.next
              textEnd = curLine.indexOf("</text>")
            }
            document.append(curLine.substring(0, textEnd))
          }
          // TODO: maybe toSet
          val ss = document.toString.split("[^A-Za-z0-9]").toSet
          ss.foreach(w => {
            val i = indexer.getIndex(strUnifier(w.toLowerCase))
            totalWordCounts.incrementCount(i, 1.0)
            currentWordCounts += i
            //currentWordCounts(i) += 1
            //currentWordCounts.incrementCount(i, 1.0)
          })
        }
      }
    }

    // get the 300 most common words and remove them from all the documents
    /*val wrdsq = totalWordCounts.asPriorityQueue
    val removeWords = new mutable.HashSet[Int]()
    for(i <- 0 until 300; if wrdsq.hasNext)
      removeWords += wrdsq.next
    for(k <- documentResults) {
      documentResults(k._1) = k._2.filter(!removeWords.contains(_)).sorted
    }*/
    val contextWords = new mutable.HashMap[String,IntArray]()
    for(k <- documentResults) {
      // get top 300 based off tf-idf score and indicators for the documents
      // the seralization is suppose to notice when two objects are the same
      // so these should just become the same arrays in memory
      if(k._2.size > 300)
        contextWords(k._1) = IntArray.makeDiskBacked(k._2.toArray.sortBy(w => {
          totalWordCounts.getCount(w)
        }).slice(0, 300).sorted)
      else
        contextWords(k._1) = k._2

    }
    val cntArr = IntArray.makeDiskBacked(IntArray.makeArray(totalWordCounts.size))
    for(i <- 0 until totalWordCounts.size) {
      cntArr(i) = totalWordCounts.getCount(i).asInstanceOf[Int]
    }

    // this works, but just .... ga
    /*var allArrays = documentResults.map(_._2) ++ contextWords.map(_._2) ++ Seq(cntArr)
    val intArrays = IntArray.combineArraysMapped(allArrays.toSeq)
    allArrays = null
    var at = 0
    // the order of the iterable should still be the same
    for(k <- documentResults) {
      assert(k._2.size == intArrays(at).size)
      documentResults(k._1) = intArrays(at)
      at += 1
    }
    for(k <- contextWords) {
      assert(k._2.size == intArrays(at).size)
      contextWords(k._1) = intArrays(at)
      at += 1
    }
    cntArr = intArrays(at)
    assert(at + 1 == intArrays.size)
    */

    new WikipediaTextDB(indexer, documentResults, contextWords, cntArr)
  }
}
