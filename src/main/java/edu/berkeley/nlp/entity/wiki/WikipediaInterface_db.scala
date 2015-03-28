package edu.berkeley.nlp.entity.wiki

import scalikejdbc._

/**
 * Created by matthewfl
 */
class WikipediaInterface_db (conn : String) {

  val settings = ConnectionPoolSettings(
    initialSize = 5,
    maxSize = 20,
    connectionTimeoutMillis = 3000L,
    validationQuery = "select 1")
  ConnectionPool.add(this, conn, "wiki", "wiki", settings)

  NamedDB(this) localTx { implicit session =>
    sql"select 1".map(a=>a).single.apply()
  }

  def disambigRes(query: Query) = {
    Seq[String]()
  }

}


object WikipediaInterface_db {
  def main(args : Array[String]): Unit = {
    // simple db test
    var db = new WikipediaInterface_db("jdbc:postgresql://10.7.0.17/wiki")
  }
}