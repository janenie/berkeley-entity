package edu.berkeley.nlp.entity.wikivec

import java.io._

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import org.deeplearning4j.text.sentenceiterator.labelaware.LabelAwareSentenceIterator
import org.deeplearning4j.text.sentenceiterator.{SentenceIterator, SentencePreProcessor}

import scala.annotation.tailrec
import scala.collection.JavaConversions._

/**
 * Created by matthewfl
 */

abstract class WikipediaPage
case class WikipediaRedirectPage(val title: String, val to: String) extends WikipediaPage
case class WikipediaNormalPage(val title: String, val content: String) extends WikipediaPage

class WikipediaInputStream(val wikiDumpPath: String) extends SentenceIterator with LabelAwareSentenceIterator {

  private var wikiIterator: Iterator[String] = null

  override def reset(): Unit = {
    wikiIterator = IOUtils.lineIterator(new BufferedReader(new InputStreamReader(/*try {
      // try and read the raw wiki dump in bzip2 format
      new CompressorStreamFactory().createCompressorInputStream(
        new BufferedInputStream(new FileInputStream(new File(wikiDumpPath))))
    } catch {
      case e: CompressorException => new FileInputStream(new File(wikiDumpPath))
    }*/
    new FileInputStream(new File(wikiDumpPath))
    ), 1024*1024*4 ))
  }

  // load in the wikipedia document for the first time
  reset

  private def getNextPage: WikipediaPage = {
    var title: String = null
    val text = new StringBuilder
    var redirect: String = null
    var doneWithPage = false
    // skip to the start of the page
    while(wikiIterator.hasNext && !wikiIterator.next().contains("<page>")) {}
    while(wikiIterator.hasNext && !doneWithPage) {
      val line = wikiIterator.next
      if(line.contains("</page>")) {
        doneWithPage = true
      } else if(line.contains("<title>")) {
        title = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>")).replace(" ", "_")
      } else if(line.contains("<redirect title")) {
        val startIdx = line.indexOf("\"") + 1;
        val endIdx = line.indexOf("\"", startIdx);
        redirect = line.substring(startIdx, endIdx)
        doneWithPage = true
      } else if(line.contains("<text>")) {
        val textStart = line.indexOf(">") + 1
        var textEnd = line.indexOf("</text>")
        if(textEnd != -1) {
          text.append(line.substring(textStart, textEnd))
        } else {
          var curLine = line.substring(textStart)
          while(textEnd == -1) {
            text.append(curLine)
            curLine = wikiIterator.next
            textEnd = curLine.indexOf("</text>")
          }
          text.append(curLine.substring(0, textEnd))
        }
      }
    }
    if(title == null) {
      null
    } else if(redirect != null) {
      new WikipediaRedirectPage(title, redirect)
    } else {
      new WikipediaNormalPage(title, text.toString)
    }
  }

  private var currentPage: WikipediaPage = null

  private var currentSentenceIterator: Iterator[String] = null

  @tailrec
  private def loadNextPage: Unit = {
    currentPage = getNextPage
    currentPage match {
      case null => {}
      case WikipediaRedirectPage(_,_) => {
        loadNextPage
      }
      case WikipediaNormalPage(title, content) => {
        currentSentenceIterator = content.toLowerCase.split("[\\.\\?\\!\n]").iterator
      }
    }
  }

  override def hasNext: Boolean = {
    if(currentPage != null) return true
    loadNextPage
    currentPage != null
  }


  override def finish: Unit = {
    // TODO:
  }

  private var preprocessor: SentencePreProcessor = new SentencePreProcessor {
    override def preProcess(sentence: String): String = sentence
  }


  override def getPreProcessor = preprocessor

  override def setPreProcessor(p: SentencePreProcessor) = preprocessor = p

  override def currentLabel: String = currentPage match {
    case null => null
    case WikipediaNormalPage(title, _) => title
    case WikipediaRedirectPage(title, _) => title
  }

  override def currentLabels: java.util.List[String] = List(currentLabel)

  override def nextSentence: String = {
    if(currentSentenceIterator == null || !currentSentenceIterator.hasNext)
      loadNextPage
    currentSentenceIterator.next()
  }

}
