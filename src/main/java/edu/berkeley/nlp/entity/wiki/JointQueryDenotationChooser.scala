package edu.berkeley.nlp.entity.wiki

import java.io.File

import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.wikivec.{ExternalWikiProcessor, w2vReader}
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.joint.LikelihoodAndGradientComputer
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.futile.math.SloppyMath
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.joint.GeneralTrainer

/**
 * Keeps track of queries and an associated set of denotations, some of which are
 * correct for this particular example. Note that queries contain information about
 * the mentions they were derived from (but may need to be augmented to store even
 * more for more sophisticated feature computation).
 */
case class JointQueryDenotationExample(val queries: Seq[Query],
                                       val allDenotations: Seq[String],
                                       val correctDenotations: Seq[String],
                                       val rawCorrectDenotations: Seq[String],
                                       val allDocDenotations: Seq[String]) {

  val correctDenotationIndices = allDenotations.zipWithIndex.filter(denotationAndIdx => correctDenotations.contains(denotationAndIdx._1)).map(_._2)

  // Feature caches since feature computation is expensive if redone every time
  var cachedFeatsEachQuery: Array[Array[Int]] = null;
  var cachedFeatsEachQueryDenotation: Array[Array[FeatureRep]] = null;

  def document = queries.head.originalMent.rawDoc

  // return a set of other links that are from other documents
  // TODO: this isn't quite right, as if another links is to the same document we will filter it
  // whereas the origional wikification system would have keep it
  def otherLinks = allDocDenotations.filter(!correctDenotations.contains(_))

  def makeDocCache(wikiDB: WikipediaInterface) = {
    if(document.documentVectorCache == null) {
      document.documentVectorCache = wikiDB.textDB.makeVector(document.words)
      document.contextVectorCache = wikiDB.textDB.makeContextVector(document.documentVectorCache)
    }
  }


  def clearDocCache = {
    document.documentVectorCache = null
    document.contextVectorCache = null
  }
}

/**
 * "Inferencer" that takes examples and weight vectors and does relevant operations: computes train
 * likelihoods, computes gradients, and computes the best denotation for an example given a set of
 * parameters.
 */
class JointQueryDenotationChoiceComputer(val wikiDB: WikipediaInterface,
                                         val featureIndexer: Indexer[String],
                                         val word2vec: w2vReader,
                                         val externalWiki: ExternalWikiProcessor) extends LikelihoodAndGradientComputer[JointQueryDenotationExample] {
  // Used for feature computation
  val queryChooser = new QueryChoiceComputer(wikiDB, featureIndexer)

  def featurizeUseCache(ex: JointQueryDenotationExample, addToIndexer: Boolean, useGoldKnowledge: Boolean, isTraining: Boolean = false) {
    if (ex.cachedFeatsEachQuery == null) {
      /*if(ex.document.documentVectorCache == null) {
        ex.document.documentVectorCache = wikiDB.textDB.makeVector(ex.document.words)
        //ex.document.contextVectorCache = wikiDB.textDB.makeContextVector(ex.document.documentVectorCache)
      }*/
      ex.cachedFeatsEachQuery = queryChooser.featurizeQueries(ex.queries, addToIndexer)
      ex.cachedFeatsEachQueryDenotation = queryChooser.featurizeQueriesAndDenotations_GLOW(
        ex.queries, ex.allDenotations, addToIndexer, wikiDB, ex.otherLinks, word2vec, externalWiki, isTraining,
        if(ex.correctDenotations.isEmpty) null else ex.correctDenotations(0),
        ex.cachedFeatsEachQuery)
//      val r = queryChooser.featurizeQueriesAndDenotations(ex.queries, ex.allDenotations, addToIndexer)
//      ex.cachedFeatsEachQueryDenotation = r.map(_.map(FeatureRep.makeFeatureRep(_)))
    }
  }

  /**
   * Computes a score matrix for (query, denotation) pairs based on the computed features
   */
  def getUnnormalizedJointScores(ex: JointQueryDenotationExample, weights: Array[Float]): Array[Array[Float]] = {
    featurizeUseCache(ex, false, useGoldKnowledge = true, isTraining = JointQueryDenotationChooser.isTraining) // TODO: change
    // each example will have a number of features associated with each query
    // each feature is an indicator, so we use the cache of the features indexes
    // and sum the values of the features
    val rawQueryScores = ex.cachedFeatsEachQuery.map(feats => if(feats != null) GUtil.scoreIndexedFeats(feats, weights) else 0f);
    // these are the weights from each query wrt the various word choices
    val queryDenotationMatrix = ex.cachedFeatsEachQueryDenotation.map(_.map(feats => feats.dotWeights(weights)));
    val scores = Array.tabulate(ex.queries.size, ex.allDenotations.size)((i, j) => Float.NegativeInfinity)
    for (queryIdx <- 0 until ex.queries.size; denotationIdx <- 0 until ex.allDenotations.size) {
      // These are indicator weights, so by summing them we can compute the resulting value of choosing a given word
      // and a given query by combining the results of the dot product of the query and the denotation

      // these values are in log space, so the sum here is a multiply, and the values should be propto probs
      // but tha rawQS and qDM are created from summing over their features,
      scores(queryIdx)(denotationIdx) = rawQueryScores(queryIdx) + queryDenotationMatrix(queryIdx)(denotationIdx)
    }
    scores
  }

  /**
   * Computes denotation marginals (normalized, but still in log space). Query choice
   * gets marginalized out here.
   */
  def getDenotationLogMarginals(ex: JointQueryDenotationExample, weights: Array[Float]): Array[Float] = {
    val scores = getUnnormalizedJointScores(ex, weights)
    // the scores matrix contains log(p_{i,j}), so we are using
    // logAdd to sum the probabilities
    // as p(q,d) \propto e^(w^T f(q,d))
    val rawDenotationMarginals = Array.tabulate(ex.allDenotations.size)(i => SloppyMath.logAdd(scores.map(_(i))).toFloat)
    val normalizer = SloppyMath.logAdd(rawDenotationMarginals).toFloat
    (0 until rawDenotationMarginals.size).foreach(i => rawDenotationMarginals(i) -= normalizer)
    rawDenotationMarginals
  }

  /**
   * Computes the gradient on the given example at the point specified by the given weight vector and adds it to thea
   * gradient array.
   */
  def addUnregularizedStochasticGradient(ex: JointQueryDenotationExample, weights: Array[Float], gradient: Array[Float]) {
    // False for adding features here, though it doesn't really matter; we'd better have cached all of them
    // in advance anyway to know how long the weight vector should be

    // todo:mfl: seems like there might be something wrong here.......
    val allFeats = featurizeUseCache(ex, false, useGoldKnowledge = true, isTraining = JointQueryDenotationChooser.isTraining) // TODO: change to false
    val scores = getUnnormalizedJointScores(ex, weights)
    val logNormalizer = SloppyMath.logAdd(scores.map(SloppyMath.logAdd(_)))
    var goldLogNormalizer = Float.NegativeInfinity
    for (j <- ex.correctDenotationIndices) {
      for (i <- 0 until ex.queries.size) {
        goldLogNormalizer = SloppyMath.logAdd(goldLogNormalizer, scores(i)(j)).toFloat
      }
    }
    for (i <- 0 until ex.queries.size) {
      for (j <- 0 until ex.allDenotations.size) {
        val isCorrect = ex.correctDenotationIndices.contains(j)
        val goldCount = if (isCorrect) (Math.exp(scores(i)(j) - goldLogNormalizer)).toFloat else 0F
        val predCount = Math.exp(scores(i)(j) - logNormalizer).toFloat
        ex.cachedFeatsEachQueryDenotation(i)(j).addToGradient(goldCount - predCount, gradient)
        //GUtil.addToGradient(ex.cachedFeatsEachQueryDenotation(i)(j), goldCount - predCount, gradient);
        GUtil.addToGradient(ex.cachedFeatsEachQuery(i), goldCount - predCount, gradient);
      }
    }
  }

  /**
   * Computes the log likelihood. Since there are multiple correct answers, we have to take a sum.
   */
  def computeLogLikelihood(ex: JointQueryDenotationExample, weights: Array[Float]): Float = {
    val denotationMarginals = getDenotationLogMarginals(ex, weights)
    val correctScores = ex.correctDenotationIndices.map(denotationMarginals(_)).toArray;
    SloppyMath.logAdd(correctScores).toFloat;
  }

  /**
   * Finds the highest-scoring denotation as the answer
   */
  def computeDenotation(ex: JointQueryDenotationExample, weights: Array[Float]) = {
    val denotationMarginals = getDenotationLogMarginals(ex, weights)
    ex.allDenotations(GUtil.argMaxIdxFloat(denotationMarginals));
  }
}

/**
 * Pairs a feature indexer with a set of weights; note that the WikipediaInterface is excluded
 * so that it doesn't get serialized in (because it's big) and also so this can be ported
 * to another dataset with a different set of pre-extracted sufficient statistics.
 */
class JointQueryDenotationChooser(val featureIndexer: Indexer[String],
                                  val weights: Array[Float]) extends Serializable {

  /*def pickDenotation(queries: Seq[Query], wikiDB: WikipediaInterface): String = {
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featureIndexer);
    val denotations = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val ex = new JointQueryDenotationExample(queries, denotations, Array[String](), Array[String]());
    computer.computeDenotation(ex, weights)
  }*/

  def pickDenotations(queries: Seq[Query], wikiDB: WikipediaInterface, otherLinks: Seq[String], word2vec: w2vReader, externalWikiProcessor: ExternalWikiProcessor, gold: String) : (Seq[(String, Int)], Array[Array[Int]]) = {
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featureIndexer, word2vec, externalWikiProcessor);
    val denotations = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val dden = Query.extractDenotationSetWithNil(queries, denotations, JointQueryDenotationChooser.maxNumWikificationOptions)
    val ex = new JointQueryDenotationExample(queries, dden, Array[String](gold), Array[String](), otherLinks);
    val denotationMarginals = computer.getDenotationLogMarginals(ex, weights)

    (ex.allDenotations.zipWithIndex.sortBy(v => denotationMarginals(v._2)).reverse,
      ex.cachedFeatsEachQuery)
  }

  def printEverything(queries: Seq[Query], wikiDB: WikipediaInterface, word2vec: w2vReader, externalWikiProcessor: ExternalWikiProcessor, correctInd: Int, otherLinks: Seq[String]) = {
    // just redo the computations so gg
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featureIndexer, word2vec, externalWikiProcessor);
    val denotations = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val dden = Query.extractDenotationSetWithNil(queries, denotations, JointQueryDenotationChooser.maxNumWikificationOptions)
    val ex = new JointQueryDenotationExample(queries, dden, Array[String](), Array[String](), otherLinks);
    val denotationMarginals = computer.getDenotationLogMarginals(ex, weights)

    val sortedItms = ex.allDenotations.zipWithIndex.sortBy(v => denotationMarginals(v._2)).reverse

    println(
      s"""Correct item in $correctInd (${sortedItms(correctInd)._1})
         |\tGuessed value: ${sortedItms(0)._1}""".stripMargin)
    for(i <- 0 until queries.length) {
      println("\t\t"+i+": "+queries(i))
      println("\t\t"+ex.cachedFeatsEachQuery(i).map(featureIndexer.getObject(_)).mkString(" "))
      for(j <- 0 until ex.allDenotations.length) {
        println("\t\t\t"+j+": "+ex.allDenotations(j)+": "+
          ex.cachedFeatsEachQueryDenotation(i)(j).intFeatures.map(i => featureIndexer.getObject(i)).mkString(" ")
        // TODO: print the weights for the weighted features
        )
      }
    }
    println()
  }

}

object JointQueryDenotationChooser {

  /**
   * Extracts an isolated set of entity linking examples from coreference documents
   * and standoff entity link annotations.
   */
  def extractExamples(corefDocs: Seq[CorefDoc], goldWikification: CorpusWikiAnnots, wikiDB: WikipediaInterface, filterImpossible: Boolean = false) = {
    val exs = new ArrayBuffer[JointQueryDenotationExample];
    var numImpossible = 0;
    // Go through all mentions in all documents
    for (corefDoc <- corefDocs) {
      val docName = corefDoc.rawDoc.docID
      val allGold = corefDoc.predMentions.map(d => {
        // There are multiple possible gold Wikipedia titles for some mentions. Note that
        // NIL (no entry in Wikipedia) is included as an explicit choice, so this includes NILs (as
        // it should according to how the task is defined)
        val g = getGoldWikification(goldWikification(docName), d)
        (g ++ g.map(wikiDB.redirectsDB.followRedirect(_))).distinct
      })
      val allGoldSet = allGold.flatMap(v => v).toSet.toSeq
      for (i <- 0 until corefDoc.predMentions.size) {
        // Discard "closed class" mentions (pronouns) since these don't have interesting entity links
        if (!corefDoc.predMentions(i).mentionType.isClosedClass()) {
          val ment = corefDoc.predMentions(i);

          //val goldLabelp = getGoldWikification(goldWikification(docName), ment)
          //val goldLabel = (goldLabelp ++ goldLabelp.map(wikiDB.redirectsDB.followRedirect(_))).distinct
          val goldLabel = allGold(i)
          if (goldLabel.size >= 1) {
            //val oldqueries = Query.extractQueriesBest_old(ment, true);
            val queries = Query.extractQueriesBest(ment, true);
            /*if(!(Set(oldqueries.map(_.getFinalQueryStr):_*) subsetOf Set(queries.map(_.getFinalQueryStr):_*))) {
              println("failed...")
            }*/
            //val queryDisambigs = queries.map(wikiDB.disambiguateBestGetAllOptions(_));
//            val denotations = queries.map(wikiDB.disambiguateBestNoDisambig(_));
            val queryDisambigs = queries.map(wikiDB.disambigRes(_))
            val denotations = Query.extractDenotationSetWithNil(queries, queryDisambigs, maxNumWikificationOptions);
            val correctDenotations = denotations.filter(denotation => isCorrect(goldLabel, denotation))
            // N.B. The use of "isCorrect" here is needed to canonicalize
            val correctIndices = denotations.zipWithIndex.filter(denotationIdx => isCorrect(goldLabel, denotationIdx._1)).map(_._2);
//            if (correctIndices.isEmpty &&
            if (filterImpossible && correctIndices.isEmpty) {
              numImpossible += 1;
              //println("impossible: "+goldLabel +"\n\tqueries: "+queries+"\n\tdisamb: "+queryDisambigs+"\n\tdentations: "+denotations)
              /*if(goldLabel.contains("Lord_Speaker")) {
                println("wtfwtf")
              }*/
            } else {
              exs += new JointQueryDenotationExample(queries, denotations, correctDenotations, goldLabel, allGoldSet)
            }
          }
        }
      }
    }
    Logger.logss(exs.size + " possible, " + numImpossible + " impossible");
    exs;
  }


  def loadDocuments(path : String) = {
    val limit = numLoadedSamples//500
    if(path.startsWith("wikiser:")) {
      WikiDocReader.loadRawWikiDocs(path.split(":")(1), limit, "", Language.ENGLISH)
    } else {
      ConllDocReader.loadRawConllDocsWithSuffix(path, limit, "", Language.ENGLISH)
    }
  }


  val trainDataPath = "data/ace05/train";
  val testDataPath = "data/ace05/dev";
  val wikiPath = "data/ace05/ace05-all-conll-wiki" // contains the wiki links for both items
  val wikiDBPath = "models/wiki-db-ace.ser.gz"
  val w2vectors = "enwiki-vectors.bin"
  val externalWikiProcess = "external-wiki.json"
  val saveExternalWikiProcess = ""

  val lambda = 1e-8F
  val batchSize = 1
  val numItrs = 20

  val maxNumWikificationOptions = 20 //7

  val numLoadedSamples = -1 // for debugging by loading less samples

  var isTraining = true // DO NOT SET, AND FML

  def main(args: Array[String]) {
    LightRunner.initializeOutput(JointQueryDenotationChooser.getClass());
    LightRunner.populateScala(JointQueryDenotationChooser.getClass(), args)
    // Read in CoNLL documents
    val assembler = CorefDocAssembler(Language.ENGLISH, true);
    val trainDocs = loadDocuments(trainDataPath);
    val trainCorefDocs = trainDocs.map(doc => {
      try {
        assembler.createCorefDoc(doc, new MentionPropertyComputer(None))
      } catch {
        case e : Exception => {
          // TODO: fix the wikidocument parser
          println("failed document "+doc.docID)
          null
        }
      }
    }).filter(_!=null);

    //val testDocs = ConllDocReader.loadRawConllDocsWithSuffix(testDataPath, -1, "", Language.ENGLISH);
    val testDocs = loadDocuments(testDataPath)
    val testCorefDocs = testDocs.map(doc => assembler.createCorefDoc(doc, new MentionPropertyComputer(None)));

    // Read in gold Wikification labels
    val goldWikification = WikiAnnotReaderWriter.readStandoffAnnotsAsCorpusAnnots(wikiPath)
    // Read in the title given surface database
    IntArray.prefixDir = new File(wikiDBPath).getParent
    val wikiDB = GUtil.load(wikiDBPath).asInstanceOf[WikipediaInterface];
    // disable using w2v directly since it does not appear to work that well
    // without additional support
    val word2vec: w2vReader = null //new w2vReader(w2vectors)
    val externalWiki = new ExternalWikiProcessor(wikiDB, externalWikiProcess)
    // Make training examples, filtering out those with solutions that are unreachable because
    // they're not good for training
    var trainExs = extractExamples(trainCorefDocs, goldWikification, wikiDB, filterImpossible = true)

    // going to have make this system work on a set of a document

    // Extract features
    val featIndexer = new Indexer[String]
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featIndexer, word2vec, externalWiki);
    var lastDocument : Document = null
    isTraining = true
    for (trainEx <- trainExs) {
      if(trainEx.document != lastDocument) {
        if(lastDocument != null) {
          // clear the cache of these features for memory
          lastDocument.contextVectorCache = null
          lastDocument.documentVectorCache = null
        }
        lastDocument = trainEx.document
      }
      trainEx.makeDocCache(wikiDB)
      computer.featurizeUseCache(trainEx, true, useGoldKnowledge = true, isTraining = true);
    }
    Logger.logss(featIndexer.size + " features");
    // Train
    val gt = new GeneralTrainer[JointQueryDenotationExample]();
    val weights = gt.trainAdagrad(trainExs, computer, featIndexer.size, 1.0F, lambda, batchSize, numItrs);

    val chooser = new JointQueryDenotationChooser(featIndexer, weights)

    // reclaim mem
    trainExs = null

    // Build the test examples and decode the test set
    // No filtering now because we're doing test

    isTraining = false // we are not longer looking at training examples

    val testExs = extractExamples(testCorefDocs, goldWikification, wikiDB, filterImpossible = true)//false);

    println("feature weights:")
    weights.zipWithIndex.sortBy(v => Math.abs(v._1)).foreach(v =>{
      //val vv = FeatureRep.mapToOrgValue(v._2)
      //if(vv != -1)
      println(featIndexer.getObject(v._2)+": "+v._1)
    })
    println()

    var correctItemWasInSet = 0

    val results = testExs.map(t => {
      // TODO: need more then one perdicted title
      t.makeDocCache(wikiDB)

      val (picks, denFeats) = chooser.pickDenotations(t.queries, wikiDB, t.otherLinks, word2vec = word2vec, externalWikiProcessor = externalWiki, gold = t.rawCorrectDenotations(0)) // TOOD: remove hack
      if(!isCorrect(t.rawCorrectDenotations, picks(0)._1)) {
        // the pick is not correct, attempt to determine if there would have
        // been a better pick that is in the picks list (which basically means all of the
        /*if(picks.size > 1 && isCorrect(t.rawCorrectDenotations, picks(1))) {
          // the correct pick was the second answer instead of the first one
          // try and report the differences between the two items
          println("second pick was correct")

        }*/
        var qq = -1
        for((p, i) <- picks.zipWithIndex) {
          // try: t.correctDenotations here?
          if(isCorrect(t.correctDenotations, p._1) || isCorrect(t.rawCorrectDenotations, p._1)) {
            //println("Found correct item with "+i)
            correctItemWasInSet += 1
            qq = i
            //println("found correct item")
          }
        }
        /*if(qq != -1) {
          //chooser.printEverything(t.queries, wikiDB, qq, t.otherLinks)

          println(
            s"""Correct tem in place: $qq
                |\tcorrect value: ${picks(qq)}
                |\t\t${denFeats(picks(qq)._2).flatMap(featIndexer.getObject(_)).mkString(" ")}
                |\tchosen value : ${picks(0)}
                |\t\t${denFeats(picks(0)._2).flatMap(featIndexer.getObject(_)).mkString(" ")}
              """.stripMargin)

        } else {
          println("THIS QUERY SHOULD HAVE BEEN FILTERED")
        }*/
      }
      (t.rawCorrectDenotations, picks.map(_._1), t.queries(0).originalMent.rawDoc)
    })

    val goldTestDenotationsAsTrivialChunks = (0 until results.size).map(i => new Chunk[Seq[String]](i, i+1, results(i)._1))
    val predTestDenotationsAsTrivialChunks = (0 until results.size).map(i => new Chunk[String](i, i+1, results(i)._2(0)))

    // Hacky but lets us reuse some code that normally evaluates things with variable endpoints
//    WikificationEvaluator.evaluateWikiChunksBySent(Seq(goldTestDenotationsAsTrivialChunks), Seq(predTestDenotationsAsTrivialChunks))
    WikificationEvaluator.evaluateFahrniMetrics(Seq(goldTestDenotationsAsTrivialChunks), Seq(predTestDenotationsAsTrivialChunks), Set())

    val mentionsByDoc = results.groupBy(_._3)

    WikificationEvaluator.evaluateBOTF1_mfl(mentionsByDoc)
    println("Number of correct items that were in the set: "+correctItemWasInSet)

    if(!saveExternalWikiProcess.isEmpty)
      externalWiki.save(saveExternalWikiProcess, featIndexer)


    LightRunner.finalizeOutput();
  }

}
