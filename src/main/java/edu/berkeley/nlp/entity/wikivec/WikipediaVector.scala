package edu.berkeley.nlp.entity.wikivec


import java.util.concurrent._

import org.deeplearning4j.bagofwords.vectorizer.TfidfVectorizer
import org.deeplearning4j.models.embeddings.inmemory.InMemoryLookupTable
import org.deeplearning4j.models.word2vec.Word2Vec
import org.deeplearning4j.models.word2vec.wordstore.inmemory.InMemoryLookupCache
import org.deeplearning4j.text.documentiterator.DocumentIterator
import org.deeplearning4j.text.invertedindex.LuceneInvertedIndex
import org.deeplearning4j.text.sentenceiterator.SentenceIterator
import org.deeplearning4j.text.tokenization.tokenizerfactory.UimaTokenizerFactory


/**
 * Created by matthewfl
 */
class WikipediaVector(sentenceIterator: SentenceIterator = null,
                      documentIterator: DocumentIterator = null,
                      learningRate: Double = 2.5e-2,
                      iterations: Int = 1) extends Word2Vec {

  // set fields from super class
  sentenceIter = sentenceIterator
  docIter = documentIterator
  alpha.set(learningRate)
  numIterations = iterations

  if(tokenizerFactory == null)
    tokenizerFactory = new UimaTokenizerFactory()

  if(vocab == null)
    vocab = new InMemoryLookupCache()

  if(lookupTable == null) {
    lookupTable = new InMemoryLookupTable.Builder()
      .negative(1)
      .useAdaGrad(useAdaGrad)
      .lr(alpha.doubleValue())
      .cache(vocab)
      .vectorLength(layerSize)
      .build()
  }



  override  def fit = {
    buildVocab()

    if(stopWords == null) readStopWords()

    val exec: ExecutorService = new ThreadPoolExecutor(Runtime.getRuntime.availableProcessors,
      Runtime.getRuntime.availableProcessors, 0L,
      TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], new RejectedExecutionHandler() {
      def rejectedExecution(r: Runnable, executor: ThreadPoolExecutor) {
        try {
          Thread.sleep(1000)
        }
        catch {
          case e: InterruptedException => {
            Thread.currentThread.interrupt
          }
        }
        executor.submit(r)
      }
    })


    //vectorizer.index().eachDocWithLabels()

  }

  override def buildVocab(): Boolean =  {
    readStopWords()

    if (vocab.vocabExists) {
      Word2Vec.log.info("Loading vocab...")
      vocab.loadVocab
      lookupTable.resetWeights
      return true
    }

    if (invertedIndex == null)
      invertedIndex = new LuceneInvertedIndex.Builder().cache(vocab).stopWords(stopWords).build
    //vectorizer will handle setting up vocab meta data
    if (vectorizer == null) {
      vectorizer = new TfidfVectorizer.Builder()
        .index(invertedIndex)
        .cache(vocab)
        .iterate(docIter)
        .iterate(sentenceIter)
        .batchSize(batchSize)
        .minWords(minWordFrequency)
        .stopWords(stopWords)
        .tokenize(tokenizerFactory)
        .build
      vectorizer.fit
    }
    else if (vocab.numWords < 2) vectorizer.fit

    true
  }






}

object WikipediaVector {

  /*
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

  def builder = new Builder
  */

}

