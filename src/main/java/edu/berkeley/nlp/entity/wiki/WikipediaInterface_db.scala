package edu.berkeley.nlp.entity.wiki

import scalikejdbc._

/**
 * Created by matthewfl
 */
class WikipediaInterface_db (conn : String) {


  Class.forName("org.postgresql.Driver")
  val settings = ConnectionPoolSettings(
    initialSize = 5,
    maxSize = 20,
    connectionTimeoutMillis = 3000L,
    validationQuery = "select 1")
  ConnectionPool.add(this, conn, "wiki", "wiki", settings)

  using(DB(ConnectionPool.borrow(this))) { db =>
    db localTx { implicit session =>
      SQL("select 1 as i").map(r=>r.int(0)).single.apply()
    }
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