package edu.berkeley.nlp.entity.wikivec

/*
import org.deeplearning4j.models.embeddings.inmemory.InMemoryLookupTable
import org.deeplearning4j.models.word2vec.Word2Vec
import org.deeplearning4j.models.word2vec.wordstore.inmemory.InMemoryLookupCache
import org.deeplearning4j.text.tokenization.tokenizerfactory.UimaTokenizerFactory


/**
 * Created by matthewfl
 */
class WikipediaVector extends Word2Vec {

  override  def fit = {
    buildVocab()

  }

  override def buildVocab(): Boolean =  {


    true
  }






}

object WikipediaVector {


  class Builder extends Word2Vec.Builder {

    override def build = {
      val ret = new WikipediaVector
      ret.window = window
      ret.alpha.set(lr)
      ret.vectorizer = textVectorizer
      ret.stopWords = stopWords
      ret.setVocab(vocabCache)
      ret.numIterations = iterations
      ret.minWordFrequency = minWordFrequency
      ret.seed = seed
      ret.saveVocab = saveVocab
      ret.batchSize = batchSize
      ret.useAdaGrad = useAdaGrad
      ret.minLearningRate = minLearningRate
      ret.sample = sampling
      ret.workers = workers
      ret.invertedIndex = index
      ret.lookupTable = lookupTable

      try {
        if (tokenizerFactory == null)
          tokenizerFactory = new UimaTokenizerFactory
      }
      catch {
        case e: Exception => {
          throw new RuntimeException(e)
        }
      }

      if (vocabCache == null) {
        vocabCache = new InMemoryLookupCache
        ret.setVocab(vocabCache)
      }

      if (lookupTable == null) {
        lookupTable = new InMemoryLookupTable.Builder().negative(negative).useAdaGrad(useAdaGrad).lr(lr).cache(vocabCache).vectorLength(layerSize).build
      }

      ret.docIter = docIter
      ret.lookupTable = lookupTable
      ret.tokenizerFactory = tokenizerFactory



      ret
    }

  }

}

 */