package edu.berkeley.nlp.entity.wikivec

import java.io._

import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import org.json.{JSONArray, JSONTokener, JSONObject}

import scala.collection.mutable
import scala.collection.JavaConversions._

/**
 * Created by matthewfl
 */
class ExternalWikiProcessor(val wikiInterface: WikipediaInterface, val queryDB: String=null) {

  /**
   * source -> ( (surface text) -> (training, Gold, [(possible target, score)]))
   * the known gold value is only used when training isnce we are taking these items and
   * then running training over these
   *
   * the source should represent the text from the document that this is coming from since
   * the title of a document does not make since as we will not know that during testing time
   */

  // TODO: support context around a link
  type documentType = mutable.HashMap[String, (Boolean, String, Array[Int], Map[String, (Float, Array[Int])])]
  type queryType = mutable.HashMap[String, documentType]


  val queries = {
    if(queryDB != null && !queryDB.isEmpty) {
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
    val ret = doc.getOrElseUpdate(surface, (training, knownGold, null, possibles.map(p => (p, (0f, null))).toMap))._3
    ret
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
        //qJ.put("")
        if(q._2._2 == null)
          qJ.put("gold", "") // somehow we don't know what the gold label is here?
        else
          qJ.put("gold", q._2._2)
        val gvals = new JSONObject()
        qJ.put("vals", gvals)
        for(m <- q._2._4) {
          val iarr = new JSONArray()
          iarr.put(m._2._1)
          iarr.put(m._2._2) // hopefully putting an array will work
          gvals.put(m._1,iarr)
        }
      }
    }

    val f = new OutputStreamWriter(/*new GZIPOutputStream*/(new FileOutputStream(fname)))
    f.write(base.toString())
    f.close()
  }

  def load(fname: String=queryDB) = {
    val ret = new queryType
    val jsontokenizer = new JSONTokener(new FileInputStream(fname))
    val base = new JSONObject(jsontokenizer)
    val queriesJ = base.getJSONObject("results")
    for(docKey <- queriesJ.keys()) {
      val docJ = queriesJ.getJSONObject(docKey)
      val doc = ret.getOrElseUpdate(docKey.asInstanceOf[String], new documentType)
      for(qurKey <- docJ.keys()) {
        val qur = docJ.getJSONObject(qurKey)
        val training = qur.getBoolean("training")
        val gold = qur.getString("gold")
        val gvals = qur.getJSONObject("vals")
        val mvals = gvals.keys.map(k => {
          (k, gvals.getDouble(k).asInstanceOf[Float])
        }).toMap
        doc.put(qurKey, (training, gold, mvals))
      }
    }
    ret
  }


}
