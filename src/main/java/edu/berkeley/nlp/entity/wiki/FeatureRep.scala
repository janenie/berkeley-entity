package edu.berkeley.nlp.entity.wiki

import scala.collection.mutable.ArrayBuffer

/**
 * Created by matthewfl
 */
sealed trait FeatureRep {

  def intFeatures: Array[Int]

  def weightedFeatures: Array[Int]

  def weightedFeaturesWeights: Array[Float]

}

object FeatureRep {
  val numWeightedFeatures = 300
}


class FeatureBuilder extends FeatureRep {

  private val intFeaturesB = new ArrayBuffer[Int]()

  override def intFeatures = intFeaturesB.toArray

  private val weightedFeaturesB = new ArrayBuffer[Int]()

  override def weightedFeatures = weightedFeaturesB.toArray

  private val weightedFeaturesWeightsB = new ArrayBuffer[Float]()

  override def weightedFeaturesWeights = weightedFeaturesWeightsB.toArray

  def makeFinal: FeatureRep = {
    new FinalFeature(intFeaturesB.toArray,
      weightedFeaturesB.toArray,
      weightedFeaturesWeightsB.toArray)
  }

  def addIndicator(id: Int) = {
    assert(id >= FeatureRep.numWeightedFeatures)
    intFeaturesB += id
  }

  def addWeighted(id: Int, v: Float) = {
    assert(id < FeatureRep.numWeightedFeatures)
    weightedFeaturesWeightsB += v
    weightedFeaturesB += id
  }


}

sealed class FinalFeature (val intFeatures: Array[Int],
                    val weightedFeatures: Array[Int],
                    val weightedFeaturesWeights: Array[Float])
  extends FeatureRep {

}