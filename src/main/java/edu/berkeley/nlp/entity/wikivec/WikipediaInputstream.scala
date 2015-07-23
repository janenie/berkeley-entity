/*package edu.berkeley.nlp.entity.wikivec

import java.io._

import edu.berkeley.nlp.entity.wiki.WikipediaRedirectsDB
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

class WikipediaInputStream(val wikiDumpPath: String,
                           val wikipediaRedirects: WikipediaRedirectsDB) extends SentenceIterator with LabelAwareSentenceIterator {

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
    ), 1024 * 1024 * 4))
  }

  // load in the wikipedia document for the first time
  reset

  private def getNextPage: WikipediaPage = {
    var title: String = null
    val text = new StringBuilder
    var redirect: String = null
    var doneWithPage = false
    // skip to the start of the page
    while (wikiIterator.hasNext && !wikiIterator.next().contains("<page>")) {}
    while (wikiIterator.hasNext && !doneWithPage) {
      val line = wikiIterator.next
      if (line.contains("</page>")) {
        doneWithPage = true
      } else if (line.contains("<title>")) {
        title = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>")).replace(" ", "_")
      } else if (line.contains("<redirect title")) {
        val startIdx = line.indexOf("\"") + 1;
        val endIdx = line.indexOf("\"", startIdx);
        redirect = line.substring(startIdx, endIdx)
        doneWithPage = true
      } else if (line.contains("<text")) {
        val textStart = line.indexOf(">") + 1
        var textEnd = line.indexOf("</text>")
        if (textEnd != -1) {
          text.append(line.substring(textStart, textEnd))
        } else {
          var curLine = line.substring(textStart)
          while (textEnd == -1) {
            text.append(curLine)
            if (!wikiIterator.hasNext)
              return null
            curLine = wikiIterator.next
            textEnd = curLine.indexOf("</text>")
          }
          text.append(curLine.substring(0, textEnd))
        }
      }
    }
    if (title == null) {
      null
    } else if (redirect != null) {
      new WikipediaRedirectPage(title, redirect)
    } else {
      new WikipediaNormalPage(title, text.toString)
    }
  }

  private var currentPage: WikipediaPage = null

  private var currentSentenceIterator: Iterator[String] = null

  private var currentProcessedText: String = null

  @tailrec
  private def loadNextPage: Unit = {
    currentPage = getNextPage
    currentPage match {
      case null => {}
      case WikipediaRedirectPage(_, _) => {
        loadNextPage
      }
      case WikipediaNormalPage(title, content) => {
        //val wm = new WikiModel("", "")
        //wm.render(new PlainTextConverter(), content)
        currentProcessedText = linksToTokens(
          content
            .replaceAll("&lt;.+?&gt;", "")
            .replaceAll("\\|thumb", "")
            .replaceAll("\\|left", "")
            .replaceAll("\\|right", "")
            .replaceAll("\\|\\d+px", "")
        ).replaceAll("\\{\\{.+?\\}\\}", "")
          .replaceAll("[^A-Za-z0-9_\\.\\?\\!\n]", " ")
          .replaceAll("[ \\t\\x0B\\f\\r]+", " ")
          .replaceAll("\\n+", "\\n")
          .toLowerCase
        currentSentenceIterator = currentProcessedText.split("[\\.\\?\\!\n]").iterator
      }
    }
  }

  private def linksToTokens(s: String) = {
    val buf = new StringBuffer()
    var lastPlace = 0
    var startIdx = s.indexOf('[')
    while (startIdx >= 0) {
      if (s.charAt(startIdx + 1) == '[') {
        // this is an internal link of the form [[the page|the surface text]]
        var endIdx = s.indexOf("]]", startIdx)
        if (s.indexOf('[', startIdx) < endIdx) {
          // there is some [ inside this link
          // such a hack, but try and match up the two items
          endIdx = startIdx + 2
          var cnt = 2
          while (cnt > 0 && endIdx < s.length) {
            s.charAt(endIdx) match {
              case '[' => {
                cnt += 1
              }
              case ']' => {
                cnt -= 1
              }
              case _ => {}
            }
            endIdx += 1
          }
        }
        if (endIdx == -1)
          endIdx = s.length
        val pipeIdx = s.indexOf('|', startIdx)
        val linkDest = if (pipeIdx >= 0 && pipeIdx < endIdx) {
          s.substring(startIdx + 2, pipeIdx);
        } else if (endIdx >= startIdx + 2) {
          s.substring(startIdx + 2, endIdx);
        } else {
          ""
        }
        if (!linkDest.isEmpty) {
          buf.append(s.substring(lastPlace, startIdx))
          buf.append(" ")
          buf.append(wikipediaRedirects.followRedirect(linkDest).replace(' ', '_').replace("(", "_LRB_").replace(")", "_RRB_"))
          buf.append(" ")
          lastPlace = endIdx + 2
        }
        startIdx = s.indexOf('[', endIdx + 2)
      } else {
        // this is an external link of the form [http://adsfasdf.com some surface text]
        var endIdx = s.indexOf(']', startIdx)
        val sep = s.indexOf(' ', startIdx)
        buf.append(s.substring(lastPlace, startIdx))
        if (endIdx == -1)
          endIdx = s.length
        if (sep != -1 && sep < endIdx)
          buf.append(s.substring(sep, endIdx))
        startIdx = s.indexOf('[', endIdx + 1)
        lastPlace = endIdx + 1
      }
    }

    if (lastPlace < s.length)
      buf.append(s.substring(lastPlace, s.length))
    buf.toString
  }

  override def hasNext: Boolean = {
    if (currentSentenceIterator != null)
      if (currentSentenceIterator.hasNext)
        return true
    loadNextPage
    currentSentenceIterator.hasNext
  }

  override def finish: Unit = {
    // TODO:
  }

  private var preprocessor: SentencePreProcessor = new SentencePreProcessor {
    override def preProcess(sentence: String): String = sentence
  }


  override def getPreProcessor = preprocessor

  override def setPreProcessor(p: SentencePreProcessor) = preprocessor = p

  override def currentLabel: String = {
    val s = currentPage match {
      case null => null
      case WikipediaNormalPage(title, _) => title
      case WikipediaRedirectPage(title, _) => title
    }
    if (s != null) {
      s.replace(' ', '_').replace("(", "_LRB_").replace(")", "_RRB_").toLowerCase()
    } else null
  }

  override def currentLabels: java.util.List[String] = List(currentLabel)

  override def nextSentence: String = {
    if (currentSentenceIterator == null || !currentSentenceIterator.hasNext)
      loadNextPage
    try {
      currentSentenceIterator.next()
    } catch {
      case e: NoSuchElementException => null
    }
  }

  def dumpTo(path: String): Unit = {
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(path))))

    while (hasNext) {
      try {
        loadNextPage
        output.write(currentProcessedText)
        output.write("\n")
      } catch {
        case e: StringIndexOutOfBoundsException => {}
      }

    }

    output.close()
  }

}
*/