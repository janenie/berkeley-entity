package edu.berkeley.nlp.entity

import scala.collection.mutable

/**
 * Created by matthewfl
 *
 * Strings are imutable, and we potentially will have the same titles in multiple places,
 * so we want all instances of a string to point at the same string to reduce memory usage
 *
 * This is used when creating a seralized object, however we don't need to store it since
 * the seralization system will already consider if an object has been seralized before and combine
 * their references when reconstructing the object
 */
class StringUnifier {

  private val strs = new mutable.HashMap[String,String]()

  def apply(v: String): String = {
    strs.get(v) match {
      case Some(s) => s
      case None => {
        strs += (v -> v)
        v
      }
    }
  }

}
