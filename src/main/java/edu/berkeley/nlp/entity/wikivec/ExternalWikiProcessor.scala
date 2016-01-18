package edu.berkeley.nlp.entity.wikivec

import java.io._
import java.util

import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.futile.fig.basic.Indexer
import org.json.{JSONArray, JSONObject}

import scala.collection.JavaConversions._
import scala.collection.mutable

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
  case class SurfaceMatchTarget(val score: Float, var targetParam: Array[Array[Int]]) // target params is [queries][target]
  case class SurfaceQueries(val queryParams: Array[Int])
  case class SurfaceMatch(val training: Boolean, val gold: Seq[String], var queryParams: Array[Int], val targets: Map[String, SurfaceMatchTarget], val queries: util.ArrayList[SurfaceQueries])
  type documentType = mutable.HashMap[String, SurfaceMatch]
  //type documentType = mutable.HashMap[String, (Boolean, String, Array[Int], Map[String, (Float, Array[Int])])]
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

  def lookup(from: String, surface: String, possibles: Seq[String], knownGold: Seq[String], training: Boolean) = {
    val doc = queries.getOrElseUpdate(from, new documentType)
    val ret = doc.getOrElseUpdate(surface, new SurfaceMatch(training, knownGold, null, possibles.map(p => (p, new SurfaceMatchTarget(0f, null))).toMap, new util.ArrayList[SurfaceQueries]()))
    //val ret = doc.getOrElseUpdate(surface, (training, knownGold, null, possibles.map(p => (p, (0f, null))).toMap))._4
    (ret.targets, ret.queries)
  }

  def save(fname: String=queryDB, indexer: Indexer[String]) = {
    val pages = new mutable.HashSet[String]()
    val base = new JSONObject()
    val queriesJ = new JSONObject()
    base.put("queries", queriesJ)
    base.put("featIndex", indexer.getObjects.asInstanceOf[util.ArrayList[String]].toArray)
    for(doc <- queries) {
      val docJ = new JSONObject()
      queriesJ.put(doc._1, docJ)
      for(q <- doc._2) {
        val qJ = new JSONObject()
        docJ.put(q._1, qJ)
        qJ.put("training", q._2.training)
        //qJ.put("")
        if(q._2.gold == null)
          qJ.put("gold", Array[String]()) // somehow we don't know what the gold label is here?
        else
          qJ.put("gold", q._2.gold.toArray)
        val gvals = new JSONObject()
        qJ.put("vals", gvals)
        for(m <- q._2.targets) {
          val iarr = new JSONArray()
          iarr.put(m._2.score)
          iarr.put(m._2.targetParam) // hopefully putting an array will work
          gvals.put(m._1, iarr)
        }
        val qvals = new JSONArray()
        qJ.put("query_vals", qvals)
        for(i <- q._2.queries) {
          qvals.put(i.queryParams)
        }
      }
    }

    val f = new OutputStreamWriter(/*new GZIPOutputStream*/(new FileOutputStream(fname)))
    base.write(f)
    //f.write(base.toString())
    f.close()
  }

  def load(fname: String=queryDB) = {
    ???
//    val ret = new queryType
//    val jsontokenizer = new JSONTokener(new FileInputStream(fname))
//    val base = new JSONObject(jsontokenizer)
//    val queriesJ = base.getJSONObject("results")
//    for(docKey <- queriesJ.keys()) {
//      val docJ = queriesJ.getJSONObject(docKey)
//      val doc = ret.getOrElseUpdate(docKey.asInstanceOf[String], new documentType)
//      for(qurKey <- docJ.keys()) {
//        val qur = docJ.getJSONObject(qurKey)
//        val training = qur.getBoolean("training")
//        val gold = qur.getString("gold")
//        val gvals = qur.getJSONObject("vals")
//        val mvals = gvals.keys.map(k => {
//          (k, new SurfaceMatchTarget(gvals.getDouble(k).asInstanceOf[Float], null))
//        }).toMap
//        doc.put(qurKey, new SurfaceMatch(training, gold, null, mvals))
//      }
//    }
//    ret
  }


}
