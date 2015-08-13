package edu.berkeley.nlp.entity.wikivec

import java.io._

import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import org.json.JSONObject

import scala.collection.mutable

/**
 * Created by matthewfl
 */
class ExternalWikiProcessor(val wikiInterface: WikipediaInterface, val queryDB: String=null) {

  /**
   * source -> ( (surface text) -> (training, [(possible target, (isGold, score))]))
   * the known gold value is only used when training isnce we are taking these items and
   * then running training over these
   *
   * the source should represent the text from the document that this is coming from since
   * the title of a document does not make since as we will not know that during testing time
   */

  type documentType = mutable.HashMap[String, (Boolean, Map[String, (Boolean, Float)])]
  type queryType = mutable.HashMap[String, documentType]


  val queries = {
    if(queryDB != null) {
      val f = new File(queryDB)
      if(f.exists()) {
        load()
      } else {
        new queryType()
      }
    } else {
      new queryType()
    }
  }

  def lookup(from: String, surface: String, possibles: Seq[String], knownGold: String, training: Boolean) = {
    val doc = queries.getOrElseUpdate(from, new documentType)
    doc.getOrElseUpdate(surface, (training, possibles.map(p => (p, ((p == knownGold), 0f))).toMap))
    //queries.getOrElseUpdate((from, surface), )
  }

  def save(fname: String=queryDB) = {
    val pages = new mutable.HashSet[String]()
    val base = new JSONObject()
    val queriesJ = new JSONObject()
    base.put("queries", queriesJ)
    for(doc <- queries) {
      val docJ = new JSONObject()
      queriesJ.put(doc._1, docJ)
      for(q <- doc._2) {
        val qJ = new JSONObject()
        docJ.put(q._1, qJ)
        qJ.put("training", q._2._1)

      }
    }

    val f = new OutputStreamWriter(/*new GZIPOutputStream*/(new FileOutputStream(fname)))
    f.write(base.toString())
    f.close()
  }

  def load(fname: String=queryDB) = {
    val ret = new queryType


    ret
  }


}
