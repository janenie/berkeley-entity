package edu.berkeley.nlp.entity.wikivec

import java.io.File

import edu.berkeley.nlp.entity.{IntArray, GUtil}
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.futile.LightRunner

/**
 * Created by matthewfl
 *
 * Simple run test for wikipedia vectors
 */
class SimpleRun {}

object SimpleRun {


  val wikiDump = "enwikipedia-stuff.xml"
  val wikiInterface = "wiki-interface.ser.gz"
  val savedVocab = ""

  lazy val wikipediaInterface = {
    IntArray.prefixDir = new File(wikiInterface).getParent
    GUtil.loadGz(wikiInterface).asInstanceOf[WikipediaInterface]
  }

  def main(args: Array[String]) = {
    LightRunner.initializeOutput(SimpleRun.getClass)
    LightRunner.populateScala(SimpleRun.getClass, args)



    val input = new WikipediaInputStream(wikiDump, wikipediaInterface)

    val wvec = new WikipediaVector(sentenceIterator = input)

    wvec.fit
  }

}
