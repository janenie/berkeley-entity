package edu.berkeley.nlp.entity.wiki


import edu.berkeley.nlp.futile.fig.basic.Indexer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by matthewfl
 */
sealed trait FeatureRep {

  def intFeatures: Array[Int]

  def weightedFeatures: Array[Int]

  def weightedFeaturesWeights: Array[Float]

  def dotWeights(weights: Array[Float]): Float

  def addToGradient(scale: Float, gradient: Array[Float]): Unit

  // TODO:
  def toString(ind: Indexer[String]) = ???
  // def toString = ???

}

object FeatureRep {
  val numWeightedFeatures = 300

  private var nextWeightedIndex = 0

  private val weightMap = new mutable.HashMap[Int,Int]()

  private val weightMapReverse = new mutable.HashMap[Int,Int]()

  def getWeightedMap(id: Int) = {
    weightMap.getOrElseUpdate(id, {
      nextWeightedIndex += 1
      assert(nextWeightedIndex < numWeightedFeatures)
      weightMapReverse += ((nextWeightedIndex - 1, id))
      nextWeightedIndex - 1
    })
  }

  def mapToOrgValue(v: Int): Int = {
    if(v < numWeightedFeatures) {
      weightMapReverse.getOrElse(v, -1)
    } else {
      if(weightMap.contains(v)) {
        -1
      } else {
        v - numWeightedFeatures
      }
    }
  }

}


class FeatureBuilder extends FeatureRep {

  private val intFeaturesB = new ArrayBuffer[Int]()

  override def intFeatures = intFeaturesB.toArray

  private val weightedFeaturesB = new ArrayBuffer[(Int,Float)]()

  override def weightedFeatures = weightedFeaturesB.map(_._1).toArray

  override def weightedFeaturesWeights = weightedFeaturesB.map(_._2).toArray

  def makeFinal: FeatureRep = {
    val weighted = weightedFeaturesB.sortBy(_._1)
    new FinalFeature(intFeaturesB.toArray.sorted,
      weighted.map(_._1).toArray,
      weighted.map(_._2).toArray
    )
  }

  def addIndicator(id: Int) = {
    //assert(id >= FeatureRep.numWeightedFeatures)
    intFeaturesB += (id + FeatureRep.numWeightedFeatures)
  }

  def += (id: Int) = {
    intFeaturesB += id
  }

  def addWeighted(id: Int, v: Float) = {
    val nid = FeatureRep.getWeightedMap(id)
    weightedFeaturesB += ((nid, v))
  }

  override def dotWeights(weights: Array[Float]) = {
    var ret = 0F
    var i = 0
    while(i < intFeaturesB.size) {
      ret += weights(intFeaturesB(i))
      i += 1
    }
    i = 0
    while(i < weightedFeaturesB.size) {
      val a = weightedFeaturesB(i)
      ret += weights(a._1) * a._2
      i += 1
    }
    ret
  }

  override def addToGradient(scale: Float, gradient: Array[Float]): Unit = {
    var i = 0
    while(i < intFeaturesB.size) {
      gradient(intFeaturesB(i)) += scale
      i += 1
    }
    i = 0
    while(i < weightedFeaturesB.size) {
      val a = weightedFeaturesB(i)
      gradient(a._1) += scale * a._2
    }
  }

}

sealed class FinalFeature (val intFeatures: Array[Int],
                           val weightedFeatures: Array[Int],
                           val weightedFeaturesWeights: Array[Float])
  extends FeatureRep {

  override def dotWeights(weights: Array[Float]) = {
    var ret = 0F
    var i = 0
    while(i < intFeatures.length) {
      ret += weights(intFeatures(i))
      i += 1
    }
    i = 0
    while(i < weightedFeatures.length) {
      ret += weights(weightedFeatures(i)) * weightedFeaturesWeights(i)
      i += 1
    }
    ret
  }

  override def addToGradient(scale: Float, gradient: Array[Float]): Unit = {
    var i = 0
    while(i < intFeatures.length) {
      gradient(intFeatures(i)) += scale
      i += 1
    }
    i = 0
    while(i < weightedFeatures.length) {
      gradient(weightedFeatures(i)) += scale * weightedFeaturesWeights(i)
      i += 1
    }
  }

}