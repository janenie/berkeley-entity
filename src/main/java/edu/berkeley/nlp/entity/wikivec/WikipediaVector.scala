package edu.berkeley.nlp.entity.wikivec


import java.util
import java.util.Random
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicLong

import akka.actor.ActorSystem
import org.deeplearning4j.bagofwords.vectorizer.TfidfVectorizer
import org.deeplearning4j.models.embeddings.inmemory.InMemoryLookupTable
import org.deeplearning4j.models.{word2vec, paragraphvectors}
import org.deeplearning4j.models.paragraphvectors.ParagraphVectors
import org.deeplearning4j.models.word2vec.{VocabWord, Word2Vec}
import org.deeplearning4j.models.word2vec.wordstore.inmemory.InMemoryLookupCache
import org.deeplearning4j.parallel.Parallelization
import org.deeplearning4j.text.documentiterator.DocumentIterator
import org.deeplearning4j.text.invertedindex.LuceneInvertedIndex
import org.deeplearning4j.text.sentenceiterator.SentenceIterator
import org.deeplearning4j.text.tokenization.tokenizerfactory.UimaTokenizerFactory
import org.deeplearning4j.berkeley.Pair
import com.google.common.base

import scala.collection.JavaConversions._


/**
 * Created by matthewfl
 */
class WikipediaVector(sentenceIterator: SentenceIterator = null,
                      documentIterator: DocumentIterator = null,
                      learningRate: Double = 2.5e-2,
                      iterations: Int = 1,
                       _layerSize: Int = 100) extends Word2Vec {

  type SentenceLabeledType = Pair[util.List[VocabWord], util.Collection[VocabWord]]
  type RandomIterator = AtomicLong

  // set fields from super class
  sentenceIter = sentenceIterator
  docIter = documentIterator
  alpha.set(learningRate)
  numIterations = iterations
  layerSize = _layerSize
  saveVocab = false // some bug when reloading the saved vocab


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
    lookupTable.resetWeights(true)
  }



  override  def fit = {
    val loaded = buildVocab()

    if(!loaded && saveVocab)
      vocab.saveVocab()

    if(stopWords == null)
      readStopWords()

    val nextRandom = new RandomIterator(0) //new java.util.Random()

    totalWords = vectorizer.numWordsEncountered() * numIterations

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

    val batch2 = new ConcurrentLinkedDeque[SentenceLabeledType]()


    // make batches out of all sentence examples?
    vectorizer.index().eachDocWithLabels(new base.Function[Pair[util.List[VocabWord], util.Collection[String]], Void] {
      override def apply(input: Pair[util.List[VocabWord], util.Collection[String]]): Void = {
        val batch = new util.ArrayList[VocabWord]()
        // appears to add the sentence to the batch
        // this is the place sampling happens if there are a number of common words
        for(w <- input.getFirst)
          if(w != null)
            batch.add(w)

        //addWords(input.getFirst, nextRandom, batch)

        if(batch.isEmpty)
          return null

        val labels = new util.ArrayList[VocabWord]()
        for(s <- input.getSecond) {
          labels.add(vocab.wordFor(s))
        }
        batch2.add(new SentenceLabeledType(batch, labels))

        null
      }
    }, exec)


    exec.shutdown
    try {
      exec.awaitTermination(1, TimeUnit.DAYS)
    }
    catch {
      case e: InterruptedException => {
        Thread.currentThread.interrupt
      }
    }

    val numWordsSoFar = new AtomicLong(0)

    for(i <- 0 until numIterations)
      doIteration(batch2, numWordsSoFar, nextRandom)

  }

  override def buildVocab(): Boolean =  {
    readStopWords()

    /*
    // TODO: some bug
    if (vocab.vocabExists) {
      Word2Vec.log.info("Loading vocab...")
      vocab.loadVocab
      lookupTable.resetWeights
      return true
    }
    */

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

    false
  }

  /**
   * Discard frequent words?
   * That
   * @param sentence
   * @param random
   * @param currMiniBatch
   *
  protected def addWords(sentence: List[VocabWord], random: RandomIterator, currMiniBatch: java.util.List[VocabWord]): Unit = {
    for(wrd <- sentence) {
      if(wrd != null)
        currMiniBatch.add(wrd)
    }
  }*/


  protected def doIteration(batch2: util.Queue[SentenceLabeledType],
                            numWordsSoFar: AtomicLong,
                            nextRandom: RandomIterator): Unit = {
    val actorSystem = ActorSystem.create()
    Parallelization.iterateInParallel(batch2, new Parallelization.RunnableWithParams[SentenceLabeledType] {
      override def run(sentenceWithLabel: SentenceLabeledType, args: Array[Object]): Unit = {
        val alpha = Math.max(minLearningRate, WikipediaVector.this.alpha.get() * (1 - 1.0 * numWordsSoFar.get().asInstanceOf[Double] / totalWords))
        // TODO: logging here
        try {
          trainSentence(sentenceWithLabel, nextRandom, alpha)
        } catch {
          case e: Throwable => {
            println(e)
            e.printStackTrace()
            System.exit(1)
            throw e
          }
        }
      }
    }, actorSystem)
  }

  protected def trainSentence(sentenceWithLabel: SentenceLabeledType, random: RandomIterator, alpha: Double) = {
    if(sentenceWithLabel == null || sentenceWithLabel.getFirst.isEmpty) {}
    else {
      for(i <- 0 until sentenceWithLabel.getFirst.size()) {
        random.set(random.get()* 25214903917L + 11)
        dbow(i, sentenceWithLabel, (random.get() % window).asInstanceOf[Int], random, alpha)
      }
    }
  }

  protected def dbow(i: Int, sentenceWithLabel: SentenceLabeledType, b: Int, nextRandom: RandomIterator, alpha: Double): Unit = {
    val word = sentenceWithLabel.getFirst.get(i)
    val sentence = sentenceWithLabel.getFirst
    val labels = sentenceWithLabel.getSecond.asInstanceOf[util.List[VocabWord]] // gaaa, annoying have to have this cast from a collection to match their other method signatures

    if(word == null || sentence.isEmpty)
      return

    val end = window * 2 + 1
    for(a <- b until end) {
      if(a != window) {
        val c = i - window + a
        if(c >= 0 && c < labels.size()) {
          val lastWord = labels.get(c)
          iterate(word, lastWord, nextRandom,alpha);
        }
      }
    }

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

