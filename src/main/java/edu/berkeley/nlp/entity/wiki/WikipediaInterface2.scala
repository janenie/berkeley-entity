package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.futile.util.Counter

import scala.collection.mutable

/**
 * Created by matthewfl
 */
trait WikipediaInterface2 {

  def getStandardPriorForJointModel(ment: Mention) : Counter[String]

  def disambiguate(ment: Mention) = disambiguateBest(ment, ment.headIdx)

  def disambiguateBest(ment: Mention, specifiedHeadIdx: Int) : String

  def disambiguateBestNoDisambig(query: Query) : String

  def disambiguateBestGetAllOptions(ment: Mention, specifiedHeadIdx: Int) : Counter[String]

  def disambiguateBestGetAllOptions(query: Query) : Counter[String]

  def disambigRes(query: Query) : Counter[String]

  def disambiguateBestGetAllReasonableOptions(ment: Mention, specifiedHeadIdx: Int) : Counter[String]

  def disambiguateBestGetAllOneBestOptions(ment: Mention, specifiedHeadIdx: Int) : Counter[String]

  def getCategories(title: String) : Seq[String]

  def getCategoriesSortedByFrequency(title: String) : Seq[String]

  def getTopKCategoriesByFrequency(title: String, k: Int) : Seq[String]

  def getInfobox(title: String) : String

  def getInfoboxHead(title: String) : String

  def getAppositive(title: String) : String

  def printSome()

  def printSome(map : mutable.HashMap[String, String], message: String)
}
